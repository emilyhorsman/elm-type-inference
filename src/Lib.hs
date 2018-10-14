module Lib where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec Void String


bool :: Parser Bool
bool =
    True <$ string "True" <|> False <$ string "False"


escapedSingleQuote :: Parser Char
escapedSingleQuote =
    '\'' <$ string "\\'"


singleChar :: Parser Char
singleChar =
    between surround surround $ try escapedSingleQuote <|> noneOf "'"
    where
        surround = char '\''


