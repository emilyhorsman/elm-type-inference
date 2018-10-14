module Lib where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Utility


type Parser = Parsec Void String


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
