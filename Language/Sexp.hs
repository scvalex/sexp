module Language.Sexp (
        Sexp(..)
    ) where

data Sexp = List [Sexp] | Atom String

