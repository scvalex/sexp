sexp
====

> S-Expression parsing/printing made fun and easy

What
====

`sexp` provides an S-expression data-type, and printers and parsers
that work on all data-types that have `Generic` instances (so,
everything you're ever likely to define yourself):.

    % ghci
    GHCi, version 7.6.2: http://www.haskell.org/ghc/  :? for help

    λ > :set -XDeriveGeneric

    λ > import Language.Sexp

    λ > import GHC.Generics

    λ > data MyType = Foo { unFoo :: Int, getBar :: Double } deriving ( Show, Generic )

    λ > instance Sexpable MyType

    λ > toSexp (Foo 23 42.0)
    List [Atom "Foo",List [List [Atom "unFoo",Atom "23"],List [Atom "getBar",Atom "42.0"]]]

    λ > printMach (toSexp (Foo 23 42.0))
    "(Foo ((unFoo 23) (getBar \"42.0\")))"

    λ > parseExn (printMach (toSexp (Foo 23 42.0)))
    [List [Atom "Foo",List [List [Atom "unFoo",Atom "23"],List [Atom "getBar",Atom "42.0"]]]]

    λ > fromSexp (head (parseExn (printMach (toSexp (Foo 23 42.0))))) :: Maybe MyType
    Just (Foo {unFoo = 23, getBar = 42.0})

Installation
------------

This package is on [Hackage](http://hackage.haskell.org/package/sexp).
To install it, run:

    cabal update
    cabal install sexp
