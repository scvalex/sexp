{-# LANGUAGE Safe, OverloadedStrings, DefaultSignatures, FlexibleContexts, TypeOperators #-}

module Data.Sexp (
        Sexp(..), Sexpable(..),
        escape, unescape
    ) where

import Data.ByteString.Lazy.Char8 as BS
import GHC.Generics

-- | A 'ByteString'-based S-Expression.  You can a lazy 'ByteString'
-- with 'parse'.
data Sexp = List [Sexp] | Atom ByteString
          deriving ( Eq, Show )

class Sexpable a where
    toSexp :: a -> Sexp
    fromSexp :: (Monad m) => Sexp -> m a

    default toSexp :: (Generic a, GSexpable (Rep a)) => a -> Sexp
    toSexp x = gToSexp (from x)

class GSexpable a where
    gToSexp :: a p -> Sexp

instance GSexpable U1 where
    gToSexp _ = List []

instance (GSexpable a, GSexpable b) => GSexpable (a :*: b) where
    gToSexp (x :*: y) = List [gToSexp x, gToSexp y]

instance (GSexpable a, GSexpable b) => GSexpable (a :+: b) where
    gToSexp (L1 x) = List [Atom "left", gToSexp x]
    gToSexp (R1 x) = List [Atom "right", gToSexp x]

instance (GSexpable a) => GSexpable (M1 i c a) where
    gToSexp (M1 x) = gToSexp x

instance (Sexpable a) => GSexpable (K1 i a) where
    gToSexp (K1 x) = toSexp x

instance Sexpable Int where
    toSexp n = Atom (pack (show n))
    fromSexp (Atom s) = return $ read (unpack s)
    fromSexp _        = fail "not an atom"

instance Sexpable Integer where
    toSexp n = Atom (pack (show n))
    fromSexp (Atom s) = return $ read (unpack s)
    fromSexp _        = fail "not an atom"

instance Sexpable Double where
    toSexp n = Atom (pack (show n))
    fromSexp (Atom s) = return $ read (unpack s)
    fromSexp _        = fail "not an atom"

instance Sexpable ByteString where
    toSexp s = Atom (escape s)
    fromSexp (Atom s) = return (unescape s)
    fromSexp _        = fail "not an atom"

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
