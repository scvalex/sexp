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
import qualified Data.ByteString as BS
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

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
    , ("atom", "a", ["a"])
    , ("singleton", "(ab)", [List ["ab"]])
    , ("atom-list", "(a b c)", [List ["a", "b", "c"]])
    , ("nested-atom-lists", "(a ((b c) (d)))", [List [ "a"
                                                     , List [ List ["b", "c"]
                                                            , List ["d"]] ] ])
    , ("atom-whitespace", "( a b  c ) (d)", [List ["a", "b", "c"], List ["d"]])
    , ("comments", "(a) ;; this is a", [List ["a"]])
    ]

basicTypeTests :: [Test]
basicTypeTests =
    concat [ typeTest "boolFalse" False "False"
           , typeTest "boolTrue" True "True"
           , typeTest "char" 'x' "x"
           , typeTest "double" (42.2 :: Double) "42.2"
           , typeTest "float" (1.33 :: Float) "1.33"
           , typeTest "int" (42 :: Int) "42"
           , typeTest "integer" (42 :: Integer) "42"
           , typeTest "bytestring" ("ana" :: ByteString) "ana"
           , typeTest "strictBytestring" ("ana" :: BS.ByteString) "ana"
           , typeTest "string" ("ana" :: String) "ana"
           , typeTest "unit" () (List [])
           , typeTest "set" (S.fromList [9 :: Int, 0, 2, 1, 0]) (List ["0", "1", "2", "9"])
           , typeTest "map" (M.fromList [("alex" :: String, 22 :: Int), ("ingrid", 21)])
                            (List [List ["alex", "22"], List ["ingrid", "21"]])
           , typeTest "intmap" (IM.fromList [(9, "A+" :: ByteString), (4, "F")])
                               (List [List ["4", "F"], List ["9", "A+"]])
           , typeTest "eitherLeft" (Left "borked" :: Either String Int) (List ["Left", "borked"])
           , typeTest "eitherRight" (Right 5 :: Either String Int) (List ["Right", "5"])
             -- FIXME This would be more normal: List ["Just", List ["23"]]
           , typeTest "maybeJust" (Just 23 :: Maybe Int) (List ["Just", "23"])
           , typeTest "maybeNothing" (Nothing :: Maybe Int) (List ["Nothing", List []])
           , typeTest "sexp" (List ["foo", List ["bar"]]) (List ["foo", List ["bar"]])
           ]
  where
    typeTest :: (Sexpable a, Show a, Eq a) => String -> a -> Sexp -> [Test]
    typeTest name x s =
        [ testCase (name ++ "To") (assertEqual "" s (toSexp x))
        , testCase (name ++ "From") (assertEqual "" (Just x) (fromSexp s)) ]

data Fallback a = None | Fallback a (Fallback a)
                deriving ( Eq, Generic, Show )

instance (Sexpable a) => Sexpable (Fallback a)

data Config = TcpConfig { useSSL :: Bool
                        , target :: ByteString
                        , port   :: Fallback Int
                        }
            | UdpConfig { udpTarget   :: (Int, Int, Int, Int)
                        , udpPorts    :: [Integer]
                        , failureRate :: Double
                        }
            | ErlangConfig ByteString ByteString ()
            | EmptyConfig
            deriving ( Eq, Generic, Show )

instance Sexpable Config

data SingleConfig = SingleConfig { getConfig :: Int }
                  deriving ( Eq, Generic, Show )

instance Sexpable SingleConfig

gTests :: [Test]
gTests = let config1 = TcpConfig True "www.google.com" (Fallback 443 (Fallback 80 None))
             config2 = UdpConfig (192, 168, 0, 1) [20, 21, 22] 0.12
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
            , singleConfigTest
            , idSingleConfigTest
            ]
  where
    manualSexpTest name config =
        testCase name (assertEqual "" (manualSexp config) (toSexp config))

    idSexpTest name config =
        testCase name (assertEqual ""
                       (Right config :: Either String Config)
                       (fromSexp (toSexp config)))

    singleConfigTest =
        testCase "singleConfig" (assertEqual "" (List [ "SingleConfig"
                                                      , List [List ["getConfig", "23"]]])
                                                (toSexp (SingleConfig 23)))

    idSingleConfigTest =
        let config = SingleConfig 23 in
        testCase "singleConfigId" (assertEqual ""
                                   (Right config :: Either String SingleConfig)
                                   (fromSexp (toSexp config)))

    manualFallbackSexp None =
        List ["None" , List []]
    manualFallbackSexp (Fallback x fb) =
        List ["Fallback", List [toSexp x, manualFallbackSexp fb]]

    manualSexp (TcpConfig s t p) = (List [ "TcpConfig"
                                         , List [ List ["useSSL", manualBoolSexp s]
                                                , List ["target", toSexp t]
                                                , List ["port", manualFallbackSexp p] ] ])
    manualSexp (UdpConfig (t1, t2, t3, t4) ps fr) =
        (List [ "UdpConfig"
              , List [ List ["udpTarget", List [toSexp t1, toSexp t2, toSexp t3, toSexp t4]]
                     , List ["udpPorts", List (map toSexp ps)]
                     , List ["failureRate", toSexp fr] ] ])
    manualSexp (ErlangConfig host cookie ()) = List ["ErlangConfig"
                                                    , List [ Atom host
                                                           , Atom cookie
                                                           , List [] ] ]
    manualSexp EmptyConfig = List ["EmptyConfig", List []]

    manualBoolSexp True = "True"
    manualBoolSexp False = "False"

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
