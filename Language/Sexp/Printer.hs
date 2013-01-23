{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Printer (
        printHum, printMach
    ) where

import Data.ByteString.Lazy.Char8 as BS hiding ( map, elem )
import Data.Sexp ( Sexp(..), escape )

-- | Pretty print a 'Sexp' with minimal formatting.  Suitable for
-- machine processing.
printMach :: Sexp -> ByteString
printMach (Atom s)  = let es = escape s
                      in if shouldQuote es
                         then BS.snoc (BS.cons '\"' es) '\"'
                         else es
  where
    shouldQuote es = BS.find (\c -> (c < 'A' || 'z' < c)
                                    && (c < '0' || '9' < c)
                                    && not (c `elem` "-_")) es /= Nothing
printMach (List xs) = BS.concat ["(", BS.intercalate " " (map printMach xs), ")"]

-- | Pretty print a 'Sexp' in a human-friendly way.
printHum :: Sexp -> ByteString
printHum = printMach
