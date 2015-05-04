{-# LANGUAGE OverloadedStrings #-}

module Language.Sexp.Printer (
        printHum, printMach
    ) where

import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Sexp ( Sexp(..), escape )
import qualified Data.ByteString.Lazy.Char8 as BS

-- | Maximum length of a list in chars for it to be on a single line.
singleLineCutoff :: Int
singleLineCutoff = 78

-- | Pretty print a 'Sexp' with minimal formatting.  Suitable for
-- machine processing.
printMach :: Sexp -> ByteString
printMach (Atom s)  = let es = escape s
                      in if shouldQuote es
                         then BS.snoc (BS.cons '\"' es) '\"'
                         else es
  where
    shouldQuote es = BS.null es
                     || BS.find (\c -> (c < 'A' || 'z' < c)
                                       && (c < '0' || '9' < c)
                                       && not (c `elem` ("-_+~<>='/*" :: String))) es /= Nothing
printMach (List xs) = makeList (map printMach xs)

-- | Turn @["a", "(b)", "c"]@ into @(a (b) c)@.
makeList :: [ByteString] -> ByteString
makeList xs = BS.snoc (BS.cons '(' (BS.intercalate " " xs)) ')'

-- | Pretty print a 'Sexp' in a human-friendly way.
printHum :: Sexp -> ByteString
printHum = BS.intercalate "\n" . fst . go
  where
    go :: Sexp -> ([ByteString], Int)
    go s@(Atom _) =
        let t = printMach s
        in ([t], fromIntegral $ BS.length t)
    go (List ss) =
        let tss = map go ss
            tss' = concat (map fst tss)
        in if all (\ts -> 1 == length (fst ts)) tss
              && sum (map snd tss) + length tss + 2 < singleLineCutoff
           then let t = makeList tss'
                in ([t], fromIntegral $ BS.length t)
           else case tss' of
               []   -> error "Human pretty-printer broken (empty case); please file an issue."
               [t1] ->
                   let t1' = makeList [t1]
                   in ([t1'], fromIntegral $ BS.length t1')
               _ ->
                   let t1 = BS.cons '(' (head tss')
                       t2 = BS.snoc (last tss') ')'
                       tss'' = concat [[t1], map (BS.cons ' ') (tail $ init tss'), [BS.cons ' ' t2]]
                   in (tss'', maximum (map (fromIntegral . BS.length) tss''))
