module Literals where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import ParserDefinition
import Utility
import Whitespace


numberLexeme :: Parser Int
numberLexeme = lexeme L.decimal


floatLexeme :: Parser Float
floatLexeme = lexeme L.float


-- We should always use this instead of numberLexeme and floatLexeme because
-- they erroneously accept values such as `4.` or `4a` and return 4.
--
-- There might be a better way of doing this?
numberLiteral :: Parser Expression
numberLiteral = do
    -- Trailing floating points are not allowed in Elm. i.e., `4.`
    candidate <- getInput
    if '.' `elem` candidate
        then Float <$> floatLexeme
        else Int <$> numberLexeme


boolLiteral :: Parser Expression
boolLiteral =
    choice
        [ Bool True <$ symbol "True"
        , Bool False <$ symbol "False"
        ]


-- TODO: #1 Handle Unicode code point \u{03BB}
escapedChar :: Parser Char
escapedChar =
    char '\\' >> choice
        [ '\n' <$ char 'n'
        , '\r' <$ char 'r'
        , '\t' <$ char 't'
        , '"' <$ char '"'
        , '\'' <$ char '\''
        , '\\' <$ char '\\'
        ] <?> "valid escape sequence: \\n, \\r, \\t, \\\", \\', \\\\"


charLiteral :: Parser Expression
charLiteral =
    fmap Char $ surroundedBy (char '\'') $ escapedChar <|> noneOf "'\\"


singleLineStringLiteral :: Parser Expression
singleLineStringLiteral =
    fmap String $
        surroundedBy (char '"') $
            many $ escapedChar <|> noneOf "\"\\\r\n"


multiLineStringLiteral :: Parser Expression
multiLineStringLiteral =
    fmap String $ surround >> manyTill (escapedChar <|> noneOf "\\") surround
  where
    surround = count 3 (char '"')
