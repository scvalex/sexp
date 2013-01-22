{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.ByteString.Lazy.Char8 as BS
import Language.Sexp ( parseExn, printHum )
import System.Console.CmdArgs

data Modes = Print { files :: [FilePath] }
           deriving ( Show, Data, Typeable )

sexpModes :: [Modes]
sexpModes =
    [ Print { files = def &= typ "FILE*" &= args }
             &= help "pretty-print S-expressions from files or stdin"
    ]
    &= program "sexp"
    &= summary "sexp v0.5 - S-Expression magic"

readPrintSexp :: FilePath -> IO ()
readPrintSexp fp = mapM_ (BS.putStrLn . printHum) . parseExn =<< BS.readFile fp

main :: IO ()
main = do
    opts <- cmdArgs $ modes sexpModes
    case opts of
        Print fs -> mapM_ readPrintSexp fs
