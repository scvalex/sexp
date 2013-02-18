{-# LANGUAGE DefaultSignatures, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, KindSignatures, TypeOperators #-}
{-# LANGUAGE FunctionalDependencies, EmptyDataDecls, UndecidableInstances #-}

-- | S-Expressions are represented by 'Sexp'.  Conversions of arbitrary types with 'Data'
-- instances are done through 'toSexp' and 'fromSexp'.
--
-- In other words, in order for 'toSexp' and 'fromSexp' to work, the type must have a
-- 'Data' instance.  This can easily be done with the @DeriveDataTypeable@ extension.n
--
-- @
-- {-# LANGUAGE DeriveDataTypeable #-}
--
-- data MyType = Foo { unFoo :: Int }
--             deriving ( Data, Show, Typeable )
-- @
--
-- Thank you @aeson@, for the model code for this module.
module Data.Sexp (
        -- * S-Expressions
        Sexp(..), toSexp, fromSexp,

        -- * Helpers
        escape, unescape
    ) where

import Control.Applicative ( Applicative(..), (<$>) )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.DList ( DList )
import Data.Monoid ( Monoid(..) )
import GHC.Generics
import Text.Printf ( printf )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.DList as DL
import qualified Data.Vector.Mutable as VM

-- | A 'ByteString'-based S-Expression.  Conceptually, a 'Sexp' is
-- either an single atom represented by a 'ByteString', or a list of
-- 'Sexp'.
data Sexp = List [Sexp] | Atom ByteString
          deriving ( Eq, Show )

class Sexpable a where
    toSexp :: a -> Sexp
    fromSexp :: (Monad m, Applicative m) => Sexp -> m a

    default toSexp :: (Generic a, GSexpable (Rep a)) => a -> Sexp
    toSexp = gToSexp . from

    default fromSexp :: (Generic a, GSexpable (Rep a), Monad m, Applicative m) => Sexp -> m a
    fromSexp s = to <$> gFromSexp s

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
    gFromRecord = maybe (fail (printf "field not found %s" key))
                        gFromSexp
                  . sLookup
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

instance (ConsSexpable a) => GSexpable (C1 c a) where
    gToSexp = consToSexp . unM1
    gFromSexp s = M1 <$> consFromSexp s

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
