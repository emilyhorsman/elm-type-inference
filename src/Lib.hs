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


-- TODO: Remember Control.Monad.Combinators.Expr
expression :: Parser Expression
expression =
    choice
        [ eitherNumberLiteral
        , Bool <$> bool
        , Char <$> singleChar
        , String <$> singleLineString
        , String <$> multiLineString
        ]


function :: Parser Function
function = do
    bindingName <- identifier
    -- TODO: Arguments.
    symbol "="
    -- TODO expression parser which then aggregates terms
    expr <- expression
    return $ BoundFunctionDefinition bindingName [] expr


numberLiteral :: Parser Int
numberLiteral = L.decimal


floatLiteral :: Parser Float
floatLiteral = L.float


-- We should always use this instead of numberLiteral and floatLiteral because
-- they erroneously accept values such as `4.` or `4a` and return 4.
--
-- There might be a better way of doing this?
eitherNumberLiteral :: Parser Expression
eitherNumberLiteral = do
    -- Trailing floating points are not allowed in Elm. i.e., `4.`
    notFollowedBy (char '.' >> eof)
    candidate <- getInput
    if '.' `elem` candidate
        then Float <$> floatLiteral
        else Int <$> numberLiteral


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
