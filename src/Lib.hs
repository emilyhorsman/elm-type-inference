module Lib where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Void
import Numeric (readDec)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Utility


type Parser = Parsec Void String


data Expression
    = Char Char
    | String String
    | Int Int
    | Float Float
    | Bool Bool
    deriving (Show, Eq)


data Function
    = BoundFunctionDefinition String [String] Expression
    deriving (Show, Eq)


-- Adhering to the convention suggested by Text.Megaparsec.Char.Lexer where
-- lexeme parsers assume no space leading a lexeme and consumes all trailing space.
--
-- General lexer method here is based on [1] and megaparsec docs.
-- [1] https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
spaceConsumer :: Parser ()
spaceConsumer =
    let
        lineComment =
            L.skipLineComment "--"
        blockComment =
            L.skipBlockCommentNested "{-" "-}"
    in
        L.space space1 lineComment blockComment


lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer


symbol = L.symbol spaceConsumer


reservedWords =
    Set.fromList
        [ "as"
        , "case"
        , "else"
        , "exposing"
        , "if"
        , "import"
        , "in"
        , "let"
        , "module"
        , "of"
        , "port"
        , "then"
        , "type"
        , "where"
        ]


identifierLeadingChar = lowerChar
identifierChar = alphaNumChar


identifier :: Parser String
identifier =
    let
        parser =
            (:) <$> identifierLeadingChar <*> many identifierChar
        failIfReservedWord word =
            if word `Set.member` reservedWords
               then fail $ "keyword " ++ word ++ " is a reserved word and cannot be an identifier"
               else return word
    in
        (lexeme . try) (parser >>= failIfReservedWord)


function :: Parser Function
function = do
    bindingName <- identifier
    -- TODO: Arguments.
    symbol "="
    -- TODO expression parser which then aggregates terms
    expr <- numberLiteral
    return $ BoundFunctionDefinition bindingName [] $ Int expr



-- I don't think we want this to be polymorphic (i.e., Parser Integral a).
-- This parser is not quite what we want, either. It currently will accept "4."
-- or even "4a" (and just parse it as 4). This will need to be figured out
-- elsewhere. Currently something like the following would return [4] instead
-- of failing.
--
-- parse (sepBy numberLiteral (char ',')) "" `shouldFailOn` "4a,4"
numberLiteral :: Parser Int
numberLiteral = L.decimal


floatLiteral :: Parser Float
floatLiteral = L.float


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
