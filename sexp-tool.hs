{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.List ( intercalate )
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
    &= summary "sexp v0.5 - S-Expression magic"

main :: IO ()
main = do
    opts <- cmdArgs $ modes sexpModes
    case opts of
        Print fs ->
            printf "Printing %s\n" (intercalate ", " fs)
