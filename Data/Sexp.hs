{-# LANGUAGE OverloadedStrings #-}

module Data.Sexp (
        Sexp(..), toSexp, fromSexp,
        escape, unescape
    ) where

import Control.Applicative
import Data.ByteString.Lazy.Char8 as BS hiding ( dropWhile, map, null, zipWith, length, elem )
import Data.Data
import Data.Generics
import Control.Monad.State ( get, put, execState, modify,
                             lift, StateT(..), evalStateT )
import Text.Printf ( printf )

-- | A 'ByteString'-based S-Expression.  You can a lazy 'ByteString'
-- with 'parse'.
data Sexp = List [Sexp] | Atom ByteString
          deriving ( Eq, Show )

-- | Convert an arbitrary value (of a type with a 'Data' instance) to
-- a 'Sexp'.  This has special cases for types with fake 'Data'
-- instances like 'ByteString'.
toSexp :: (Data a) => a -> Sexp
toSexp = genericToSexp
         `extQ` byteStringToSexp
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
                 values = gmapQ toSexp x
             in if null labels
                then values
                else zipWith fieldToSexp labels (gmapQ toSexp x)
    constrSexp =
        let constrName = showConstr c
        in if isTupleConstr constrName
           then List fields
           else List (Atom (pack constrName)
                      : if null fields then [] else fields)
    fieldToSexp name field  = List [Atom (pack name), field]

    -- FIXME Nasty hack to avoid defining `ext{3..}Q`
    isTupleConstr ('(' : rest) = dropWhile (==',') rest == ")"
    isTupleConstr _            = False

-- | Convert a 'ByteString' to a 'Sexp' by wrapping it in an 'Atom'.
byteStringToSexp :: ByteString -> Sexp
byteStringToSexp = Atom

-- | Convert a list to a 'Sexp' by converting the elements, and
-- wrapping them in a 'List'.
listToSexp :: (Data a) => [a] -> Sexp
listToSexp xs = List (map toSexp xs)

fromSexp :: (Data a, Monad m, Applicative m) => Sexp -> m a
fromSexp s = genericFromSexp s
             `extR` byteStringFromSexp s

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
                    lift $ fromSexp f

    -- Count the number of arguments of a constructor.  We can't use
    -- 'constrFields' because that only includes labeled arguments.
    numConstrArgs :: (Data a) => a -> Constr -> Int
    numConstrArgs x c = let f = do modify (+1); return undefined
                        in execState (fromConstrM f c `asTypeOf` return x) 0
genericFromSexp _ = error "genericFromSexp unknown case"

byteStringFromSexp :: (Monad m) => Sexp -> m ByteString
byteStringFromSexp (Atom bs) = return bs
byteStringFromSexp _         = fail "invalid ByteString sexp"

-- class Sexpable a where
--     toSexp :: a -> Sexp
--     fromSexp :: (Monad m, Applicative m) => Sexp -> m a

--     default toSexp :: (Generic a, GSexpable (Rep a)) => a -> Sexp
--     toSexp x = fst (gToSexp (from x))

--     default fromSexp :: (Generic a, GSexpable (Rep a), Monad m, Applicative m) => Sexp -> m a
--     fromSexp x = to <$> gFromSexp x

-- class GSexpable a where
--     gToSexp :: a p -> (Sexp, Bool)
--     gFromSexp :: (Monad m, Applicative m) => Sexp -> m (a p)

-- instance GSexpable U1 where
--     gToSexp U1 = (List [], True)

--     gFromSexp (List []) = return U1
--     gFromSexp _         = fail "not an empty constructor"

-- instance (GSexpable a, GSexpable b) => GSexpable (a :*: b) where
--     gToSexp (x :*: y) =
--         let (xs, False) = gToSexp x in
--         let (List ys, shouldConcat) = gToSexp y in
--         if shouldConcat
--         then (List (xs : ys), True)
--         else (List [xs, List ys], True)

--     gFromSexp (List (x1:x2:xs)) = do
--         (:*:) <$> gFromSexp x1 <*> gFromSexp (List (x2:xs))
--     gFromSexp _ =
--         fail "not a product type"

-- instance (GSexpable a, GSexpable b) => GSexpable (a :+: b) where
--     gToSexp (L1 x) = let (List xs, False) = gToSexp x in (List xs, False)
--     gToSexp (R1 x) = let (List xs, False) = gToSexp x in (List xs, False)

--     gFromSexp (List xs) = L1 <$> gFromSexp (List xs)
--     gFromSexp _         = fail "not a sum type"

-- instance (GSexpable a, Datatype c) => GSexpable (M1 D c a) where
--     gToSexp (M1 x) = gToSexp x
--     gFromSexp x = gFromSexp x

-- instance (GSexpable a, Selector c) => GSexpable (M1 S c a) where
--     gToSexp c@(M1 x) =
--         let (xs, _) = gToSexp x
--         in if null (selName c)
--            then (xs, False)
--            else (List [Atom (pack (selName c)), xs], False)

--     gFromSexp _ = undefined

-- instance (GSexpable a, Constructor c) => GSexpable (M1 C c a) where
--     gToSexp c@(M1 x) =
--         case gToSexp x of
--             (List [], _) -> (List [Atom (pack (conName c))], False)
--             (List xs, _) -> (List [Atom (pack (conName c)), List xs], False)
--             _            -> error "constructor case broken in generics reification"

--     gFromSexp (List (_:xs)) = M1 <$> gFromSexp (List xs)
--     gFromSexp _             = fail "not a constructor"

-- instance (Sexpable a) => GSexpable (K1 i a) where
--     gToSexp (K1 x) = (toSexp x, False)
--     gFromSexp x    = K1 <$> fromSexp x

-- instance Sexpable Bool

-- instance Sexpable Int where
--     toSexp n = Atom (pack (show n))
--     fromSexp (Atom s) = return $ read (unpack s)
--     fromSexp _        = fail "not an atom"

-- instance Sexpable Integer where
--     toSexp n = Atom (pack (show n))
--     fromSexp (Atom s) = return $ read (unpack s)
--     fromSexp _        = fail "not an atom"

-- instance Sexpable Double where
--     toSexp n = Atom (pack (show n))
--     fromSexp (Atom s) = return $ read (unpack s)
--     fromSexp _        = fail "not an atom"

-- instance Sexpable ByteString where
--     toSexp s = Atom (escape s)
--     fromSexp (Atom s) = return (unescape s)
--     fromSexp _        = fail "not an atom"

-- instance (Sexpable a, Sexpable b) => Sexpable (a, b) where
--     toSexp (x, y) = List [toSexp x, toSexp y]

-- instance (Sexpable a, Sexpable b, Sexpable c) => Sexpable (a, b, c) where
--     toSexp (x, y, z) = List [toSexp x, toSexp y, toSexp z]

-- instance (Sexpable a, Sexpable b, Sexpable c, Sexpable d) => Sexpable (a, b, c, d) where
--     toSexp (x, y, z, t) = List [toSexp x, toSexp y, toSexp z, toSexp t]

-- instance (Sexpable a) => Sexpable [a] where
--     toSexp xs = List (map toSexp xs)

-- | Escape @"@ and @\@ in the given string.  This needs to be done
-- for double-quoted atoms (e.g. @"\"Hello\", he said"@).
escape :: ByteString -> ByteString
escape = BS.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar c    = BS.singleton c

-- | The inverse of 'escape'.
unescape :: ByteString -> ByteString
unescape = BS.reverse . pack . snd . (BS.foldl' unescapeChar (False, []))
  where
    unescapeChar :: (Bool, [Char]) -> Char -> (Bool, [Char])
    unescapeChar (False, cs) '\\' = (True, cs)
    unescapeChar (_, cs) c        = (False, c : cs)
