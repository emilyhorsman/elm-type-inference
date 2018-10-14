module Lib where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char


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
    between surround surround $
        (char '\\' >> escapedChar) <|> noneOf "'\\"
    where
        surround = char '\''


