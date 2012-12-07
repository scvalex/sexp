module Language.Sexp.Parser (
        Sexp(..), parse, sexpParser
    ) where

import Control.Applicative ( (<$>), (<*), (*>), many )
import Data.Attoparsec.ByteString.Lazy ( Parser, Result(..) )
import Data.Attoparsec.ByteString.Char8 ( char, space, takeWhile1, notInClass, endOfInput )
import Data.Attoparsec.Combinator ( choice )
import qualified Data.Attoparsec.ByteString.Lazy as A
import Data.ByteString.Lazy

data Sexp = List [Sexp] | Atom ByteString
          deriving ( Show )

-- | Parse S-Expressions from a lazy 'ByteString'.  If the parse was
-- successful, @Right sexps@ is returned; otherwise, @Left (errorMsg,
-- leftover)@ is returned.
parse :: ByteString -> Either (String, ByteString) [Sexp]
parse = resultToEither . A.parse (many sexpParser <* endOfInput)
  where
    resultToEither (Done _leftover sexps)    = Right sexps
    resultToEither (Fail leftover _ctxs msg) = Left (msg, leftover)

-- | A parser for S-Expressions.  Ignoring whitespace, we follow the
-- following EBNF:
--
-- SEXP ::= '(' ATOM* ')' | ATOM
-- ATOM ::= [^ \t\n()]+
--
sexpParser :: Parser Sexp
sexpParser =
    choice [ List <$> (char '(' *> many space *> many sexpParser <* char ')') <* many space
           , atom
           ]
  where
    atom :: Parser Sexp
    atom = Atom . fromStrict <$> (takeWhile1 (notInClass " \t\n()") <* many space)
