{-# LANGUAGE DefaultSignatures, FlexibleContexts #-}

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
module Data.Sexp (
        -- * S-Expressions
        Sexp(..), toSexp, fromSexp,

        -- * Helpers
        escape, unescape
    ) where

import Control.Applicative ( (<$>) )
import Data.ByteString.Lazy.Char8 ( ByteString )
import GHC.Generics ( Generic, Rep(..), M1(..), K1(..), U1(..) )
import qualified Data.ByteString.Lazy.Char8 as BL

-- | A 'ByteString'-based S-Expression.  Conceptually, a 'Sexp' is
-- either an single atom represented by a 'ByteString', or a list of
-- 'Sexp'.
data Sexp = List [Sexp] | Atom ByteString
          deriving ( Eq, Show )

class Sexpable a where
    toSexp :: a -> Sexp
    fromSexp :: (Monad m) => Sexp -> m a

    default toSexp :: (Generic a, GSexpable (Rep a)) => a -> Sexp
    toSexp = gToSexp . from

    default fromSexp :: (Generic a, GSexpable (Rep a), Monad m, Functor m) => Sexp -> m a
    fromSexp s = to <$> gFromSexp s

----------------------
-- GHC.Generics-based generic encoding/decoding
----------------------

class GSexpable f where
    gToSexp :: f a -> Sexp
    gFromSexp :: (Monad m, Functor m) => Sexp -> m (f a)

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
