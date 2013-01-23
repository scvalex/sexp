{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.ByteString.Lazy.Char8 as BS hiding ( head )
import Data.Sexp ( Sexp )
import Data.Version ( showVersion )
import Language.Sexp ( parseExn, printHum, printMach )
import Paths_sexp ( version )
import System.Console.CmdArgs
import Text.Printf ( printf )

data Modes = Print { files   :: [FilePath]
                   , twoPass :: Bool
                   , human   :: Bool }
           deriving ( Show, Data, Typeable )

sexpModes :: [Modes]
sexpModes =
    [ Print { files   = def &= typFile &= args
            , twoPass = def &= typ "BOOL" &= help "re-parse printed S-expressions as a sanity check"
            , human   = def &= typ "BOOL" &= help "human readable S-expressions"
            } &= help "pretty-print S-expressions from files or stdin"
    ]
    &= program "sexp"
    &= summary (printf "sexp v%s - S-Expression magic" (showVersion version))

readPrintSexp :: (Sexp -> ByteString) -> FilePath -> IO ()
readPrintSexp prnt fp = mapM_ (BS.putStrLn . prnt) . parseExn =<< BS.readFile fp

readPrintReadPrintSexp :: (Sexp -> ByteString) -> FilePath -> IO ()
readPrintReadPrintSexp prnt fp =
    mapM_ (BS.putStrLn . prnt . head . parseExn . prnt) . parseExn =<< BS.readFile fp

main :: IO ()
main = do
    opts <- cmdArgs $ modes sexpModes
    case opts of
        Print fs tp hum -> do
            let prnt = if hum then printHum else printMach
            if tp
                then mapM_ (readPrintReadPrintSexp prnt) fs
                else mapM_ (readPrintSexp prnt) fs
