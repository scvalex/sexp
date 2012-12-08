{-# LANGUAGE Safe #-}

module Data.Sexp (
        Sexp(..)
    ) where

import Data.ByteString.Lazy as BS

-- | A 'ByteString'-based S-Expression.  You can a lazy 'ByteString'
-- with 'parse'.
data Sexp = List [Sexp] | Atom ByteString
          deriving ( Eq, Show )
