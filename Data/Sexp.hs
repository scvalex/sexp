{-# LANGUAGE DefaultSignatures, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, KindSignatures, TypeOperators #-}
{-# LANGUAGE FunctionalDependencies, EmptyDataDecls, UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances, ViewPatterns #-}

-- | S-Expressions are represented by 'Sexp'.  Conversion to and from arbitrary types is
-- done through 'Sexpable'.
--
-- The encoding and decoding functions from 'Sexpable', 'toSexp', and 'fromSexp' all have
-- 'Generic' default implementations.  So, if your data-type has a 'Generic' instance
-- (which you can automatically get with the @DeriveGeneric@ GHC extension), it also has a
-- 'Sexpable' instance:
--
-- @
-- {-# LANGUAGE DeriveGeneric #-}
--
-- data MyType = Foo { unFoo :: Int }
--             deriving ( Generic )
--
-- instance Sexpable MyType
--   -- the default implementation uses the 'Generic' representation of 'MyType'
-- @
--
-- If you want a specific encoding for your type, just fill in the 'Sexpable' instance
-- methods:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import Control.Applicative ( (<$>) )
--
-- data MyType = Foo { unFoo :: Int }
--
-- instance Sexpable MyType where
--     toSexp (Foo x) = List [Atom "this", Atom "is", toSexp x]
--
--     fromSexp (List [Atom "this", Atom "is", s]) = Foo <$> fromSexp s
--     fromSexp _                                  = fail "invalid MyType sexp"
-- @
--
-- Thank you, @aeson@, for the model code for this module.
module Data.Sexp (
        -- * S-Expressions
        Sexp(..), Sexpable(..),

        -- * Helpers
        escape, unescape
    ) where

import Control.Applicative ( Applicative(..), Alternative(..), (<$>) )
import Control.Monad.ST ( ST )
import Data.Bits ( shiftR )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.DList ( DList )
import Data.Monoid ( Monoid(..) )
import Data.String ( IsString(..) )
import Data.Vector ( Vector )
import GHC.Generics
import Text.Printf ( printf )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.DList as DL
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- | A 'ByteString'-based S-Expression.  Conceptually, a 'Sexp' is
-- either an single atom represented by a 'ByteString', or a list of
-- 'Sexp'.
data Sexp = List [Sexp] | Atom ByteString
          deriving ( Eq, Show )

instance IsString Sexp where
    fromString = Atom . BL.pack

class Sexpable a where
    toSexp :: a -> Sexp
    fromSexp :: (Monad m, Applicative m) => Sexp -> m a

    default toSexp :: (Generic a, GSexpable (Rep a)) => a -> Sexp
    toSexp = gToSexp . from

    default fromSexp :: (Generic a, GSexpable (Rep a), Monad m, Applicative m) => Sexp -> m a
    fromSexp s = to <$> gFromSexp s

----------------------
-- Particular Sexpable instances
----------------------

instance Sexpable Bool where
    toSexp = showToSexp
    fromSexp = readFromSexp

instance Sexpable Char where
    toSexp c = toSexp [c]
    fromSexp (Atom (BL.unpack -> [c])) = return c
    fromSexp _                         = fail "expecting char atom"

instance Sexpable Double where
    toSexp = showToSexp
    fromSexp = readFromSexp

instance Sexpable Float where
    toSexp = showToSexp
    fromSexp = readFromSexp

instance Sexpable Int where
    toSexp = showToSexp
    fromSexp = readFromSexp

instance Sexpable Integer where
    toSexp = showToSexp
    fromSexp = readFromSexp

instance Sexpable () where
    toSexp () = List []
    fromSexp (List []) = return ()
    fromSexp _         = fail "expecting unit"

instance Sexpable ByteString where
    toSexp = Atom
    fromSexp (Atom s) = return s
    fromSexp _        = fail "expecting bytestring atom"

instance Sexpable BS.ByteString where
    toSexp = Atom . BL.fromChunks . (:[])
    fromSexp (Atom s) = return (BS.concat (BL.toChunks s))
    fromSexp _        = fail "expecting bytestring atom"

instance Sexpable String where
    toSexp = Atom . BL.pack
    fromSexp (Atom s) = return (BL.unpack s)
    fromSexp _        = fail "expecting string atom"

instance (Sexpable a) => Sexpable [a] where
    toSexp xs = List (map toSexp xs)
    fromSexp (List ss) = mapM fromSexp ss
    fromSexp _         = fail "expecting list"

instance (Sexpable a, Ord a) => Sexpable (S.Set a) where
    toSexp = List . map toSexp . S.toList
    fromSexp (List ss) = S.fromList <$> mapM fromSexp ss
    fromSexp _         = fail "expecting set list"

instance (Sexpable k, Sexpable v, Ord k) => Sexpable (M.Map k v) where
    toSexp = List . map toSexp . M.toList
    fromSexp (List ss) = M.fromList <$> mapM fromSexp ss
    fromSexp _         = fail "expecting map list"

instance (Sexpable v) => Sexpable (IM.IntMap v) where
    toSexp = List . map toSexp . IM.toList
    fromSexp (List ss) = IM.fromList <$> mapM fromSexp ss
    fromSexp _         = fail "expecting map list"

instance (Sexpable a, Sexpable b) => Sexpable (a, b) where
    toSexp (x, y) = List [toSexp x, toSexp y]
    fromSexp (List [sx, sy]) = (,) <$> fromSexp sx <*> fromSexp sy
    fromSexp _               = fail "expecting 2-tuple"

instance (Sexpable a, Sexpable b, Sexpable c) => Sexpable (a, b, c) where
    toSexp (x, y, z) = List [toSexp x, toSexp y, toSexp z]
    fromSexp (List [sx, sy, sz]) = (,,) <$> fromSexp sx <*> fromSexp sy <*> fromSexp sz
    fromSexp _                   = fail "expecting 3-tuple"

instance (Sexpable a, Sexpable b, Sexpable c, Sexpable d) => Sexpable (a, b, c, d) where
    toSexp (x, y, z, u) = List [toSexp x, toSexp y, toSexp z, toSexp u]
    fromSexp (List [sx, sy, sz, su]) =
        (,,,) <$> fromSexp sx <*> fromSexp sy <*> fromSexp sz <*> fromSexp su
    fromSexp _ =
        fail "expecting 4-tuple"

instance (Sexpable a, Sexpable b, Sexpable c, Sexpable d, Sexpable e)
         => Sexpable (a, b, c, d, e) where
    toSexp (x, y, z, u, v) = List [toSexp x, toSexp y, toSexp z, toSexp u, toSexp v]
    fromSexp (List [sx, sy, sz, su, sv]) =
        (,,,,) <$> fromSexp sx <*> fromSexp sy <*> fromSexp sz <*> fromSexp su <*> fromSexp sv
    fromSexp _ =
        fail "expecting 5-tuple"

instance (Sexpable a, Sexpable b) => Sexpable (Either a b)

instance (Sexpable a) => Sexpable (Maybe a)

instance Sexpable Sexp where
    toSexp = id
    fromSexp = return

----------------------
-- What is a record?
----------------------

class IsRecord (f :: * -> *) b | f -> b

data True
data False

instance (IsRecord f b) => IsRecord (f :*: g) b
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f b) => IsRecord (M1 S c f) b
instance IsRecord (K1 i c) True
instance IsRecord U1 False

----------------------
-- Records are converted to pairs.
----------------------

type Pair = (ByteString, Sexp)

class GRecordToPairs f where
    gRecordToPairs :: f a -> DList Pair

instance (GRecordToPairs a, GRecordToPairs b) => GRecordToPairs (a :*: b) where
    gRecordToPairs (a :*: b) = gRecordToPairs a `mappend` gRecordToPairs b

instance (Selector s, GSexpable a) => GRecordToPairs (S1 s a) where
    gRecordToPairs m1 = return (BL.pack (selName m1), gToSexp (unM1 m1))

----------------------
-- Records are read from pairs.
----------------------

class GFromRecord f where
    gFromRecord :: (Monad m, Applicative m) => [Sexp] -> m (f a)

instance (GFromRecord a, GFromRecord b) => GFromRecord (a :*: b) where
    gFromRecord fs = (:*:) <$> gFromRecord fs <*> gFromRecord fs

instance (Selector s, GSexpable a) => GFromRecord (S1 s a) where
    gFromRecord s = maybe (fail (printf "field %s not found in %s" key (show s)))
                          gFromSexp
                          (sLookup s)
      where
        key = selName (undefined :: t s a p)
        keyS = Atom (BL.pack key)
        sLookup []                                      = Nothing
        sLookup (List [key', value] : _) | key' == keyS = Just value
        sLookup (_                  : fs)               = sLookup fs

----------------------
-- Record fields are converted along with their names.
----------------------

class ConsSexpable f where
      consToSexp :: f a -> Sexp
      consFromSexp :: (Monad m, Applicative m) => Sexp -> m (f a)

class ConsSexpable' b f where
      consToSexp' :: Tagged b (f a -> Sexp)
      consFromSexp' :: (Monad m, Applicative m) => Tagged b (Sexp -> m (f a))

newtype Tagged s b = Tagged { unTagged :: b }

instance (IsRecord f b, ConsSexpable' b f) => ConsSexpable f where
    consToSexp = unTagged (consToSexp' :: Tagged b (f a -> Sexp))
    consFromSexp = unTagged (consFromSexp' :: (Monad m, Applicative m) => Tagged b (Sexp -> m (f a)))

instance (GRecordToPairs f, GFromRecord f) => ConsSexpable' True f where
    consToSexp' = Tagged (List . map (\(n, s) -> List [Atom n, s]) . DL.toList . gRecordToPairs)
    consFromSexp' = Tagged fromRecord
      where
        fromRecord (List fs) = gFromRecord fs
        fromRecord _         = fail "expecting record"

instance (GSexpable f) => ConsSexpable' False f where
    consToSexp' = Tagged gToSexp
    consFromSexp' = Tagged gFromSexp

----------------------
-- Size of product types
----------------------

class ProductSize f where
    productSize :: Tagged2 f Int

newtype Tagged2 (s :: * -> *) b = Tagged2 { unTagged2 :: b }

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                            unTagged2 (productSize :: Tagged2 b Int)

instance ProductSize (S1 s a) where
    productSize = Tagged2 1

----------------------
-- Product types
----------------------

class GFromProduct f where
    gFromProduct :: (Monad m, Applicative m) => Vector Sexp -> Int -> Int -> m (f a)

instance (GFromProduct a, GFromProduct b) => GFromProduct (a :*: b) where
    gFromProduct arr ix len = (:*:) <$> gFromProduct arr ix lenL
                                    <*> gFromProduct arr ixR lenR
      where
        lenL = len `shiftR` 1
        ixR = ix + lenL
        lenR = len - lenL

instance (GSexpable a) => GFromProduct (S1 s a) where
    gFromProduct arr ix _ = gFromSexp $ V.unsafeIndex arr ix

class GProductToSexp f where
    gProductToSexp :: VM.MVector s Sexp -> Int -> Int -> f a -> ST s ()

instance (GProductToSexp a, GProductToSexp b) => GProductToSexp (a :*: b) where
    gProductToSexp mv ix len (a :*: b) = do
        gProductToSexp mv ix lenL a
        gProductToSexp mv ixR lenR b
      where
        lenL = len `shiftR` 1
        ixR = ix + lenL
        lenR = len - lenL

instance (GSexpable a) => GProductToSexp a where
    gProductToSexp mv ix _ = VM.unsafeWrite mv ix . gToSexp

----------------------
-- Sum Types
----------------------

class GFromSum f where
    gFromSum :: (Monad m, Applicative m) => Pair -> Maybe (m (f a))

instance (GFromSum a, GFromSum b) => GFromSum (a :+: b) where
    gFromSum keyVal = (fmap L1 <$> gFromSum keyVal) <|>
                      (fmap R1 <$> gFromSum keyVal)

instance (Constructor c, GSexpable a, ConsSexpable a) => GFromSum (C1 c a) where
    gFromSum (key, value)
        | key == BL.pack (conName (undefined :: t c a p)) = Just $ gFromSexp value
        | otherwise = Nothing

class GToSum f where
    gToSum :: f a -> Sexp

instance (GToSum a, GToSum b) => GToSum (a :+: b) where
    gToSum (L1 x) = gToSum x
    gToSum (R1 x) = gToSum x

instance (Constructor c, GSexpable a, ConsSexpable a) => GToSum (C1 c a) where
    gToSum x = gToSexp x

----------------------
-- GHC.Generics-based generic encoding/decoding
----------------------

class GSexpable f where
    gToSexp :: f a -> Sexp
    gFromSexp :: (Monad m, Applicative m) => Sexp -> m (f a)

instance (GSexpable a) => GSexpable (M1 i c a) where
    gToSexp = gToSexp . unM1
    gFromSexp s = M1 <$> gFromSexp s

instance (Sexpable a) => GSexpable (K1 i a) where
    gToSexp = toSexp . unK1
    gFromSexp s = K1 <$> fromSexp s

instance GSexpable U1 where
    gToSexp _ = List []
    gFromSexp (List []) = return U1
    gFromSexp _         = fail "expecting empty constructor"

instance (Constructor c, ConsSexpable a) => GSexpable (C1 c a) where
    gToSexp x = List [Atom (BL.pack (conName (undefined :: t c a p))), consToSexp (unM1 x)]

    gFromSexp (List [Atom ktr, fs])
        | ktr == BL.pack (conName (undefined :: t c a p)) = M1 <$> consFromSexp fs
    gFromSexp s = M1 <$> consFromSexp s

instance ( GProductToSexp a, GProductToSexp b
         , GFromProduct a, GFromProduct b
         , ProductSize a, ProductSize b ) => GSexpable (a :*: b) where
    gToSexp p = List $ V.toList $ V.create $ do
        mv <- VM.unsafeNew lenProduct
        gProductToSexp mv 0 lenProduct p
        return mv
      where
        lenProduct = unTagged2 (productSize :: Tagged2 (a :*: b) Int)

    gFromSexp (List xs)
        | lenList == lenProduct = gFromProduct (V.fromList xs) 0 lenProduct
        | otherwise = fail "expecting a product type (list)"
      where
        lenList = length xs
        lenProduct = unTagged2 (productSize :: Tagged2 (a :*: b) Int)
    gFromSexp (Atom _) =
        fail "expecting a product type (atom)"

instance ( GToSum a, GToSum b
         , GFromSum a, GFromSum b ) => GSexpable (a :+: b) where
    gToSexp (L1 x) = gToSum x
    gToSexp (R1 x) = gToSum x

    gFromSexp (List [Atom key, val]) =
        case gFromSum (key, val) of
            Nothing -> fail (printf "field %s not found" (show key))
            Just x  -> x
    gFromSexp _ =
        fail "expecting sum type"

----------------------
-- Helpers
----------------------

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
unescape = BL.reverse . BL.pack . snd . (BL.foldl' unescapeChar (False, []))
  where
    unescapeChar :: (Bool, [Char]) -> Char -> (Bool, [Char])
    unescapeChar (False, cs) '\\' = (True, cs)
    unescapeChar (_, cs) c        = (False, c : cs)

-- | Convert something that has a 'Show' instance to an 'Atom' with its string
-- representation.
showToSexp :: (Show a) => a -> Sexp
showToSexp = Atom . BL.pack . show

-- | The inverse of 'showToSexp'.
readFromSexp :: (Read a, Monad m, Applicative m) => Sexp -> m a
readFromSexp (Atom s) =
    case readsPrec 1 (BL.unpack s) of
        [(n, _)] -> return n
        _        -> fail "cannot read int"
readFromSexp (List _) =
    fail "expecting atom int"
