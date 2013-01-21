{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Printer (
        printHum, printMach
    ) where

import Data.ByteString.Lazy.Char8 as BS hiding ( map )
import Data.Sexp ( Sexp(..), escape )

-- | Pretty print a 'Sexp' with minimal formatting.  Suitable for
-- machine processing.
printMach :: Sexp -> ByteString
printMach (Atom s)  = BS.concat ["\"", escape s, "\""]
printMach (List xs) = BS.concat ["(", BS.intercalate " " (map printMach xs), ")"]

-- | Pretty print a 'Sexp' in a human-friendly way.
printHum :: Sexp -> ByteString
printHum = printMach
