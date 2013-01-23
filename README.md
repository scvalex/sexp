sexp
====

> S-Expression parsing/printing made fun and easy

What
====

`sexp` provides an S-expression data-type, and printers and parsers
that work on all data-types that have `Typeable` and `Data` instances.


    λ > import Language.Sexp

    λ > data MyType = Foo { unFoo :: Int } deriving ( Data, Show, Typeable )

    λ > toSexp (Foo 23)
    List [Atom "Foo",List [Atom "unFoo",Atom "23"]]

    λ > printMach (toSexp (Foo 23))
    "(Foo (unFoo 23))"

    λ > parseExn (printMach (toSexp (Foo 23)))
    [List [Atom "Foo",List [Atom "unFoo",Atom "23"]]]

    λ > fromSexp (head (parseExn (printMach (toSexp (Foo 23))))) :: Maybe MyType
    Just (Foo {unFoo = 23})
