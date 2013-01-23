{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.ByteString.Lazy.Char8 as BS hiding ( head )
import Data.Version ( showVersion )
import Language.Sexp ( parseExn, printHum )
import Paths_sexp ( version )
import System.Console.CmdArgs
import Text.Printf ( printf )

data Modes = Print { files :: [FilePath], twoPass :: Bool }
           deriving ( Show, Data, Typeable )

sexpModes :: [Modes]
sexpModes =
    [ Print { files = def &= typFile &= args
            , twoPass = def &= typ "BOOL"
            } &= help "pretty-print S-expressions from files or stdin"
    ]
    &= program "sexp"
    &= summary (printf "sexp v%s - S-Expression magic" (showVersion version))

readPrintSexp :: FilePath -> IO ()
readPrintSexp fp = mapM_ (BS.putStrLn . printHum) . parseExn =<< BS.readFile fp

readPrintReadPrintSexp :: FilePath -> IO ()
readPrintReadPrintSexp fp =
    mapM_ (BS.putStrLn . printHum . head . parseExn . printHum) . parseExn =<< BS.readFile fp

main :: IO ()
main = do
    opts <- cmdArgs $ modes sexpModes
    case opts of
        Print fs tp -> do
            if tp
                then mapM_ readPrintReadPrintSexp fs
                else mapM_ readPrintSexp fs
