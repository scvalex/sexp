{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy hiding ( map )
import Data.Monoid
import Language.Sexp
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test )

main :: IO ()
main = defaultMainWithOpts parseTests mempty

--------------------------------
-- HUnit Tests
--------------------------------

parseTests :: [Test]
parseTests = map (\(n, s, t) -> testCase n (parseTest s t)) positiveSexps

parseTest :: ByteString -> [Sexp] -> Assertion
parseTest text sexp = assertEqual "" sexp (parseExn text)

positiveSexps :: [(String, ByteString, [Sexp])]
positiveSexps =
    [ ("empty", "", [])
    , ("empty-list", "()", [List []])
    , ("empty-lists", "() ()", [List [], List []])
    , ("nested-lists", "(() ())", [List [List [], List []]])
    , ("list-whitespace", "  ( ( )( ) ) ", [List [List [], List []]])
    , ("atom", "a", [Atom "a"])
    , ("singleton", "(ab)", [List [Atom "ab"]])
    , ("atom-list", "(a b c)", [List [Atom "a", Atom "b", Atom "c"]])
    , ("nested-atom-lists", "(a ((b c) (d)))", [List [ Atom "a"
                                                     , List [ List [ Atom "b"
                                                                   , Atom "c"]
                                                            , List [Atom "d"]] ] ])
    , ("atom-whitespace", "( a b  c ) (d)", [List [ Atom "a"
                                                   , Atom "b"
                                                   , Atom "c" ]
                                             , List [Atom "d"]])
    ]
