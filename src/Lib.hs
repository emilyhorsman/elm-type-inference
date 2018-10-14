module Lib where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Void
import Numeric (readDec)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal, float)

import Utility


type Parser = Parsec Void String


-- I don't think we want this to be polymorphic (i.e., Parser Integral a).
-- This parser is not quite what we want, either. It currently will accept "4."
-- or even "4a" (and just parse it as 4). This will need to be figured out
-- elsewhere. Currently something like the following would return [4] instead
-- of failing.
--
-- parse (sepBy numberLiteral (char ',')) "" `shouldFailOn` "4a,4"
numberLiteral :: Parser Integer
numberLiteral = decimal


floatLiteral :: Parser Float
floatLiteral = float


bool :: Parser Bool
bool =
    True <$ string "True" <|> False <$ string "False"


-- TODO: #1 Handle Unicode code point \u{03BB}
escapedChar :: Parser Char
escapedChar = choice
    [ '\n' <$ char 'n'
    , '\r' <$ char 'r'
    , '\t' <$ char 't'
    , '"' <$ char '"'
    , '\'' <$ char '\''
    , '\\' <$ char '\\'
    ] <?> "valid escape sequence: \\n, \\r, \\t, \\\", \\', \\\\"


singleChar :: Parser Char
singleChar =
    surroundedBy (char '\'') $
        (char '\\' >> escapedChar) <|> noneOf "'\\"


singleLineString :: Parser String
singleLineString =
    surroundedBy (char '"') $
        many $ (char '\\' >> escapedChar) <|> noneOf "\"\\\r\n"


multiLineString :: Parser String
multiLineString =
    surround >> manyTill ((char '\\' >> escapedChar) <|> noneOf "\\") surround
    where
        surround = count 3 (char '"')
