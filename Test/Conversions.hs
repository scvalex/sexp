{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ( (<$>) )
import Data.ByteString.Lazy.Char8 hiding ( map, concat )
import Data.Monoid
import Data.Sexp
import GHC.Generics ( Generic )
import Language.Sexp
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding ( Test )
import Test.QuickCheck

main :: IO ()
main = defaultMainWithOpts tests options
  where
    tests = (concat [parseTests, basicTypeTests, idTests, gTests])
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 5000000) }) }

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
    , ("comments", "(a) ;; this is a", [List [Atom "a"]])
    ]

basicTypeTests :: [Test]
basicTypeTests =
    [ testCase "int" (assertEqual "" (Just (42 :: Int)) (fromSexp (Atom "42")))
    , testCase "integer" (assertEqual "" (Just (42 :: Integer)) (fromSexp (Atom "42")))
    , testCase "double" (assertEqual "" (Just (42.2 :: Double)) (fromSexp (Atom "42.2")))
    , testCase "string" (assertEqual "" (Just ("ana" :: ByteString)) (fromSexp (Atom "ana")))
    , testCase "boolFalse" (assertEqual "" (Atom "False") (toSexp False))
    , testCase "boolTrue" (assertEqual "" (Atom "True") (toSexp True))
    ]

data Fallback a = None | Fallback a (Fallback a)
                deriving ( Eq, Generic, Show )

instance (Sexpable a) => Sexpable (Fallback a)

data Config = TcpConfig { useSSL :: Bool
                        , target :: ByteString
                        , port   :: Fallback Int
                        }
            -- | UdpConfig { udpTarget   :: (Int, Int, Int, Int)
            | UdpConfig { udpTarget   :: (Int, Int)
                        , udpPorts    :: [Integer]
                        , failureRate :: Double
                        }
            | ErlangConfig ByteString ByteString ()
            | EmptyConfig
            deriving ( Eq, Generic, Show )

instance Sexpable Config

-- FIXME Test encoding/decoding of this.
-- data EmptyConfig

gTests :: [Test]
gTests = let config1 = TcpConfig True "www.google.com" (Fallback 443 (Fallback 80 None))
             -- FIXME Re-enable real UdpConfig, once we figure out how to handle N-tuples
             -- config2 = UdpConfig (192, 168, 0, 1) [20, 21, 22] 0.12
             config2 = UdpConfig (192, 168) [20, 21, 22] 0.12
             config3 = ErlangConfig "localhost" "chocolatechip" ()
             config4 = EmptyConfig
         in [ manualSexpTest "config1" config1
            , idSexpTest "config1id" config1
            , manualSexpTest "config2" config2
            , idSexpTest "config2id" config2
            , manualSexpTest "config3" config3
            , idSexpTest "config3id" config3
            , manualSexpTest "config4" config4
            , idSexpTest "config4id" config4
            ]
  where
    manualSexpTest name config =
        testCase name (assertEqual "" (manualSexp config) (toSexp config))

    idSexpTest name config =
        testCase name (assertEqual ""
                       (Right config :: Either String Config)
                       (fromSexp (toSexp config)))

    manualFallbackSexp None =
        List [Atom "None" , List []]
    manualFallbackSexp (Fallback x fb) =
        List [Atom "Fallback", List [toSexp x, manualFallbackSexp fb]]

    manualSexp (TcpConfig s t p) = (List [ Atom "TcpConfig"
                                         , List [ List [Atom "useSSL", manualBoolSexp s]
                                                , List [Atom "target", toSexp t]
                                                , List [Atom "port", manualFallbackSexp p] ] ])
    -- manualSexp (UdpConfig (t1, t2, t3, t4) ps fr) =
    manualSexp (UdpConfig (t1, t2) ps fr) =
        (List [ Atom "UdpConfig"
              , List [ -- List [Atom "udpTarget", List [toSexp t1, toSexp t2, toSexp t3, toSexp t4]]
                       List [Atom "udpTarget", List [toSexp t1, toSexp t2]]
                     , List [Atom "udpPorts", List (map toSexp ps)]
                     , List [Atom "failureRate", toSexp fr] ] ])
    manualSexp (ErlangConfig host cookie ()) = List [ Atom "ErlangConfig"
                                                    , List [ Atom host
                                                           , Atom cookie
                                                           , List [] ] ]
    manualSexp EmptyConfig = List [ Atom "EmptyConfig", List [] ]

    manualBoolSexp True = Atom "True"
    manualBoolSexp False = Atom "False"

--------------------------------
-- QuickCheck Properties
--------------------------------

newtype ReadableString = RS { unRS :: String }

instance Arbitrary ReadableString where
    arbitrary = sized $ \n -> do
        RS <$> sequence [ choose (' ', '~') | _ <- [1..n] ]

instance Arbitrary Sexp where
    arbitrary = sized $ \sz -> do
        n <- choose (1, 2) :: Gen Int
        case n of
            1 -> (Atom . pack . unRS) <$> arbitrary
            2 -> List <$> (resize (sz `div` 2) arbitrary)
            _ -> fail "can't touch this"

idTests :: [Test]
idTests =
    [ testProperty "idInt" (\x -> Just (x :: Int) == fromSexp (toSexp x))
    , testProperty "idInteger" (\x -> Just (x :: Integer) == fromSexp (toSexp x))
    , testProperty "idDouble" (\x -> Just (x :: Double) == fromSexp (toSexp x))
    , testProperty "idString" (\x -> Just (x :: String) == (unpack <$> fromSexp (toSexp (pack x))))
    , testProperty "stringEscape" (\x -> x == unpack (unescape (escape (pack x))))
    , testProperty "sexpPrintParse" (\x -> [x] == parseExn (printMach x))
    ]
