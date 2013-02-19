{-# LANGUAGE DeriveDataTypeable #-}

module Language.Sexp.Parser (
        Sexp(..), sexpParser,
        ParseException(..), parse, parseExn, parseMaybe
    ) where

import Control.Applicative ( (<$>), (<*), (*>), many )
import Control.Exception ( Exception )
import Data.Attoparsec.ByteString.Char8 ( char, space, notInClass, (<?>) )
import Data.Attoparsec.ByteString.Lazy ( Parser, Result(..) )
import Data.Attoparsec.Combinator ( choice )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Sexp ( Sexp(..), unescape )
import Data.Typeable ( Typeable )
import qualified Control.Exception as CE
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy.Char8 as BL

data ParseException = ParseException String ByteString
                    deriving ( Show, Typeable )

instance Exception ParseException

-- | Parse S-Expressions from a lazy 'ByteString'.  If the parse was
-- successful, @Right sexps@ is returned; otherwise, @Left (errorMsg,
-- leftover)@ is returned.
parse :: ByteString -> Either (String, ByteString) [Sexp]
parse = resultToEither . A.parse (whiteSpace *> many sexpParser)
  where
    resultToEither (Fail leftover _ctxs reason) =
        Left (reason, leftover)
    resultToEither (Done leftover sexps) =
        if BL.null leftover
        then Right sexps
        else Left ("garbage at end", leftover)

-- | A variant of 'parse' that returns 'Nothing' if the parse fails.
parseMaybe :: ByteString -> Maybe [Sexp]
parseMaybe s =
    case parse s of
        Left _      -> Nothing
        Right sexps -> Just sexps

-- | A variant of 'parse' that throws a 'ParseException' if the parse
-- fails.
parseExn :: ByteString -> [Sexp]
parseExn text =
    case parse text of
        Left (reason, leftover) -> CE.throw (ParseException reason leftover)
        Right sexps             -> sexps

-- | A parser for S-Expressions.  Ignoring whitespace, we follow the
-- following EBNF:
--
-- SEXP           ::= '(' ATOM* ')' | ATOM
-- ATOM           ::= '"' ESCAPED_STRING* '"' | [^ \t\n()]+
-- ESCAPED_STRING ::= ...
--
sexpParser :: Parser Sexp
sexpParser =
    choice [ list <?> "list"
           , atom <?> "atom"
           ]
  where
    list = List <$> (char '(' *> whiteSpace *> many sexpParser <* char ')') <* whiteSpace
    atom = Atom . unescape <$> (choice [string, anything]) <* whiteSpace
    string = BL.fromChunks . (:[]) <$> (char '"' *> AC.scan False escapedStringScanner <* char '"')
    anything = BL.fromChunks . (:[]) <$> AC.takeWhile1 (notInClass " \t\n()")

    -- Scan an escaped string.
    escapedStringScanner :: Bool -> Char -> Maybe Bool
    escapedStringScanner True  _    = Just False
    escapedStringScanner False '\\' = Just True
    escapedStringScanner False '"'  = Nothing
    escapedStringScanner False _    = Just False

-- | A parser for conventional ASCII whitespace and ";" line comments.
whiteSpace :: Parser ()
whiteSpace = do
    _ <- many space
    _ <- many comment
    return ()
  where
    comment = char ';' >> many (AC.notChar '\n') >> many space
