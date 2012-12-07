module Language.Sexp.Parser (
        Sexp(..), parse, sexpParser
    ) where

import Control.Applicative ( (<$>), (<*), (*>), many )
import Data.Attoparsec.ByteString.Lazy ( Parser, Result(..) )
import Data.Attoparsec.ByteString.Char8 ( char, space, takeWhile1, notInClass, (<?>) )
import Data.Attoparsec.Combinator ( choice )
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Lazy as BS

data Sexp = List [Sexp] | Atom ByteString
          deriving ( Show )

-- | Parse S-Expressions from a lazy 'ByteString'.  If the parse was
-- successful, @Right sexps@ is returned; otherwise, @Left (errorMsg,
-- leftover)@ is returned.
parse :: ByteString -> Either (String, ByteString) [Sexp]
parse = resultToEither . A.parse (many sexpParser)
  where
    resultToEither (Fail leftover _ctxs msg) = Left (msg, leftover)
    resultToEither (Done leftover sexps) =
        if BS.null leftover
        then Right sexps
        else Left ("garbage at end", leftover)

-- | A parser for S-Expressions.  Ignoring whitespace, we follow the
-- following EBNF:
--
-- SEXP ::= '(' ATOM* ')' | ATOM
-- ATOM ::= [^ \t\n()]+
--
sexpParser :: Parser Sexp
sexpParser =
    choice [ list <?> "list"
           , atom <?> "atom"
           ]
  where
    list = List <$> (char '(' *> many space *> many sexpParser <* char ')') <* many space
    atom = Atom . fromStrict <$> (takeWhile1 (notInClass " \t\n()") <* many space)
