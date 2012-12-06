module Language.Sexp (
        Sexp(..)
    ) where

import Data.ByteString.Lazy

data Sexp = List [Sexp] | Atom ByteString

