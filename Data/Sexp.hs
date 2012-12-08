{-# LANGUAGE Safe #-}

module Data.Sexp (
        Sexp(..), Sexpable(..)
    ) where

import Data.ByteString.Lazy.Char8 as BS

-- | A 'ByteString'-based S-Expression.  You can a lazy 'ByteString'
-- with 'parse'.
data Sexp = List [Sexp] | Atom ByteString
          deriving ( Eq, Show )

class Sexpable a where
    toSexp :: a -> Sexp
    fromSexp :: (Monad m) => Sexp -> m a

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
    toSexp s = Atom (snoc (cons '"' s) '"')
    fromSexp (Atom s) = return s
    fromSexp _        = fail "not an atom"
