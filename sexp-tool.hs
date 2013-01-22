{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.ByteString.Lazy.Char8 as BS
import Data.Version ( showVersion )
import Language.Sexp ( parseExn, printHum )
import Paths_sexp ( version )
import System.Console.CmdArgs
import Text.Printf ( printf )

data Modes = Print { files :: [FilePath] }
           deriving ( Show, Data, Typeable )

sexpModes :: [Modes]
sexpModes =
    [ Print { files = def &= typ "FILE*" &= args }
             &= help "pretty-print S-expressions from files or stdin"
    ]
    &= program "sexp"
    &= summary (printf "sexp v%s - S-Expression magic" (showVersion version))

readPrintSexp :: FilePath -> IO ()
readPrintSexp fp = mapM_ (BS.putStrLn . printHum) . parseExn =<< BS.readFile fp

main :: IO ()
main = do
    opts <- cmdArgs $ modes sexpModes
    case opts of
        Print fs -> mapM_ readPrintSexp fs
