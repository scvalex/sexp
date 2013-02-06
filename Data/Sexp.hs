{-# LANGUAGE OverloadedStrings, DefaultSignatures #-}

-- | S-Expressions are represented by 'Sexp'.  Conversions of
-- arbitrary types to and from 'Sexp' are done via the 'Sexpable' type
-- class.
--
-- The default implementation of Sexpable's 'toSexp' and 'fromSexp'
-- require the type to have a 'Data' instance.  In other words, if a
-- type is representable (which most types are), it is also
-- 'Sexpable'.
--
-- @
-- data MyType = Foo { unFoo :: Int }
--             deriving ( Data, Show, Typeable )
--
-- instance Sexpable MyType
-- @
--
module Data.Sexp (
        -- * S-Expressions
        Sexp(..), Sexpable(..),

        -- * Helpers
        escape, unescape
    ) where

import Control.Applicative
import Data.ByteString.Lazy.Char8 as BL hiding ( dropWhile, map, null, zipWith, length, elem )
import qualified Data.ByteString as BS
import Data.Data
import Data.Generics
import Control.Monad.State ( get, put, execState, modify,
                             lift, StateT(..), evalStateT )
import Text.Printf ( printf )

-- | Type class for things that can be converted to and from
-- S-Expressions.
class Sexpable a where
    toSexp :: a -> Sexp
    fromSexp :: (Monad m, Applicative m) => Sexp -> m a

    default toSexp :: (Data a) => a -> Sexp
    toSexp = toSexpDefault

    default fromSexp :: (Data a, Monad m, Applicative m) => Sexp -> m a
    fromSexp = fromSexpDefault

instance Sexpable Int

instance Sexpable Integer

instance Sexpable Double

instance Sexpable ByteString

instance Sexpable BS.ByteString

instance Sexpable Bool

-- | A 'ByteString'-based S-Expression.  Conceptually, a 'Sexp' is
-- either an single atom represented by a 'ByteString', or a list of
-- 'Sexp'.
data Sexp = List [Sexp] | Atom ByteString
          deriving ( Eq, Show )

-- | Convert an arbitrary value (of a type with a 'Data' instance) to
-- a 'Sexp'.  This has special cases for types with fake 'Data'
-- instances like 'ByteString'.
toSexpDefault :: (Data a) => a -> Sexp
toSexpDefault = genericToSexp
         `extQ` byteStringToSexp
         `extQ` strictByteStringToSexp
         `ext1Q` listToSexp

-- | Convert something with a *real* 'Data' instance to a 'Sexp'.
-- This works on algebraic data-types, primitives, but not on things
-- like 'ByteString'.
genericToSexp :: (Data a) => a -> Sexp
genericToSexp x =
    case dataTypeRep (dataTypeOf x) of
        AlgRep _ -> constrSexp
        _        -> Atom (pack (showConstr c))
  where
    c = toConstr x
    fields = let labels = constrFields c
                 values = gmapQ toSexpDefault x
             in if null labels
                then values
                else zipWith fieldToSexp labels (gmapQ toSexpDefault x)
    constrSexp =
        if isTupleConstr c
        then List fields
        else List (Atom (pack (showConstr c))
                   : if null fields then [] else fields)
    fieldToSexp name field  = List [Atom (pack name), field]

-- | Convert a 'ByteString' to a 'Sexp' by wrapping it in an 'Atom'.
byteStringToSexp :: ByteString -> Sexp
byteStringToSexp = Atom

-- | Convert a strict 'ByteString' to a 'Sexp' by wrapping it in an
-- 'Atom'.
strictByteStringToSexp :: BS.ByteString -> Sexp
strictByteStringToSexp = Atom . BL.fromChunks . (:[])

-- | Convert a list to a 'Sexp' by converting the elements, and
-- wrapping them in a 'List'.
listToSexp :: (Data a) => [a] -> Sexp
listToSexp xs = List (map toSexpDefault xs)

fromSexpDefault :: (Data a, Monad m, Applicative m) => Sexp -> m a
fromSexpDefault s = genericFromSexp s
             `extR` byteStringFromSexp s
             `extR` strictByteStringFromSexp s
             `extR` unitFromSexp s
             `ext1R` listFromSexp s
             `ext2R` tuple2FromSexp s

genericFromSexp :: forall a m. (Data a, Monad m, Applicative m) => Sexp -> m a
genericFromSexp (Atom s) = ma
  where
    ma = let s' = unpack s
         in case readConstr (dataTypeOf (undefined :: a)) s' of
             Nothing -> fail (printf "unknown atomic value: %s" s')
             Just c  -> return (fromConstr c)
genericFromSexp (List ((Atom constrName) : fields)) = ma
  where
    ma = let constrName' = unpack constrName
         in case readConstr typ constrName' of
             Nothing -> fail (printf "unknown constructor: %s" constrName')
             Just c  -> decodeArgs c fields

    typ = dataTypeOf (undefined :: a)

    decodeArgs c fs =
        let expectedArgs = numConstrArgs (undefined :: a) c
            gotArgs = length fs
        in if expectedArgs /= gotArgs
           then fail (printf "wrong number of constructor arguments: %s; expected %d; got %d"
                      (show c) expectedArgs gotArgs)
           else sortFields c fs >>= construct c

    sortFields :: Constr -> [Sexp] -> m [Sexp]
    sortFields _ []         = return []
    sortFields c fs@(f : _) =
        case f of
            List [label, _] | label `elem` (map (Atom . pack) constructorFields) ->
                -- Fields are labeled, so strip the labels, and sort
                -- them in the order of defined labels.
                mapM tupleizeField fs >>= go constructorFields
            _ ->
                -- Fields are unlabeled, so there's nothing to do.
                return fs
      where
        constructorFields = constrFields c

        tupleizeField (List [label, value]) = return (label, value)
        tupleizeField s                     = fail (printf "not a label-value pair: %s" (show s))

        go :: [String] -> [(Sexp, Sexp)] -> m [Sexp]
        go (cf : cfs) fps =
            case lookup (Atom (pack cf)) fps of
                Nothing -> fail (printf "argument %s of constructor %s not found" cf (show c))
                Just f' -> do
                    fs' <- go cfs fps
                    return (f' : fs')
        go [] _ = return []

    construct :: Constr -> [Sexp] -> m a
    construct c = evalStateT (fromConstrM constructM c)
      where
        -- FIXME Why do I need to specify "forall a" again?
        constructM :: (Data b) => StateT [Sexp] m b
        constructM = do
            fs <- get
            case fs of
                []        -> fail "ran out of constructor arguments"
                (f : fs') -> do
                    put fs'
                    lift $ fromSexpDefault f

    -- Count the number of arguments of a constructor.  We can't use
    -- 'constrFields' because that only includes labeled arguments.
    numConstrArgs :: a -> Constr -> Int
    numConstrArgs x c = let f = do modify (+1); return undefined
                        in execState (fromConstrM f c `asTypeOf` return x) 0
genericFromSexp s = fail (printf "genericFromSexp unknown case: %s" (show s))

byteStringFromSexp :: (Monad m) => Sexp -> m ByteString
byteStringFromSexp (Atom bs) = return bs
byteStringFromSexp _         = fail "invalid ByteString sexp"

strictByteStringFromSexp :: (Monad m) => Sexp -> m BS.ByteString
strictByteStringFromSexp (Atom bs) = return (BS.concat (BL.toChunks bs))
strictByteStringFromSexp _         = fail "invalid ByteString sexp"

unitFromSexp :: (Monad m) => Sexp -> m ()
unitFromSexp (List []) = return ()
unitFromSexp _         = fail "invalid unit sexp"

listFromSexp :: (Data a, Applicative m, Monad m) => Sexp -> m [a]
listFromSexp (List xs) = mapM fromSexpDefault xs
listFromSexp _         = fail "invalid list sexp"

tuple2FromSexp :: (Data a, Data b, Applicative m, Monad m) => Sexp -> m (a, b)
tuple2FromSexp (List [x1, x2]) = (,) <$> fromSexpDefault x1 <*> fromSexpDefault x2
tuple2FromSexp _               = fail "invalid tuple2 sexp"

-- | Escape @"@ and @\@ in the given string.  This needs to be done
-- for double-quoted atoms (e.g. @"\"Hello\", he said"@).
escape :: ByteString -> ByteString
escape = BL.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar c    = BL.singleton c

-- | The inverse of 'escape'.
unescape :: ByteString -> ByteString
unescape = BL.reverse . pack . snd . (BL.foldl' unescapeChar (False, []))
  where
    unescapeChar :: (Bool, [Char]) -> Char -> (Bool, [Char])
    unescapeChar (False, cs) '\\' = (True, cs)
    unescapeChar (_, cs) c        = (False, c : cs)

-- FIXME Nasty hack to avoid defining @ext{3..}Q@.
isTupleConstr :: Constr -> Bool
isTupleConstr c = go (showConstr c)
  where
    go ('(' : rest) = dropWhile (==',') rest == ")"
    go _            = False
