{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Printer (
        printHum, printMach
    ) where

import Data.Attoparsec.ByteString.Char8 ( isSpace )
import Data.ByteString.Lazy.Char8 as BS hiding ( map )
import Data.Sexp ( Sexp(..), escape )

-- | Pretty print a 'Sexp' with minimal formatting.  Suitable for
-- machine processing.
printMach :: Sexp -> ByteString
printMach (Atom s)  = let es = escape s
                      in if BS.find isSpace es /= Nothing
                         then BS.snoc (BS.cons '\"' es) '\"'
                         else es
printMach (List xs) = BS.concat ["(", BS.intercalate " " (map printMach xs), ")"]

-- | Pretty print a 'Sexp' in a human-friendly way.
printHum :: Sexp -> ByteString
printHum = printMach
