module Lib where

import Data.Char (isSpace)
import Data.Functor (void)
import qualified Data.Map.Strict as Map
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
    | List [Expression]
    | Tuple [Expression]
    | If Expression Expression Expression
    | FunctionApplication String [Expression]
    -- This should not be a constructor for Function since it is an expression.
    -- TODO: This comment is an undernuanced view.
    | AnonymousFunction [String] Expression
    | LetBinding [Function] Expression
    | Cases Expression [Case]
    | RecordValue (Map.Map String Expression)
    | RecordUpdate String (Map.Map String Expression)
    deriving (Show, Eq)


data Case
    = Case Expression Expression
    deriving (Show, Eq)


data Function
    = BoundFunctionDefinition String [String] Expression
    deriving (Show, Eq)


-- Modified pattern from Text.Megaparsec (atEnd)
didConsume p = option False $ True <$ p


-- Modified from the space1 definition.
spacePreserveNewlines :: Parser ()
spacePreserveNewlines =
    void $ takeWhile1P (Just "white space") p
  where
    p c =
        isSpace c && c /= '\n' && c /= '\r'


-- Adhering to the convention suggested by Text.Megaparsec.Char.Lexer where
-- lexeme parsers assume no space leading a lexeme and consumes all trailing space.
--
-- General lexer method here is based on [1] and megaparsec docs.
-- [1] https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
spaceConsumer :: Parser() -> Parser ()
spaceConsumer s =
    let
        lineComment =
            L.skipLineComment "--"
        blockComment =
            L.skipBlockCommentNested "{-" "-}"
    in
        L.space s lineComment blockComment


lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ spaceConsumer spacePreserveNewlines


symbol = L.symbol $ spaceConsumer spacePreserveNewlines


symbolNewline = L.symbol $ spaceConsumer space1


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
        [ numberWrapper
        , Bool <$> bool
        , Char <$> singleChar
        , String <$> singleLineString
        , String <$> multiLineString
        , List <$> listLiteral
        , Tuple <$> tupleLiteral
        , ifExpression
        , functionApplication
        , anonymousFunction
        , letBinding
        , caseExpression
        , recordValue
        , recordUpdate
        ]


ifExpression :: Parser Expression
ifExpression = do
    symbolNewline "if"
    predicate <- expression
    optional space1
    symbolNewline "then"
    trueResult <- expression
    optional space1
    symbolNewline "else"
    optional space1
    If predicate trueResult <$> expression


commaSeparatedExpressions :: String -> String -> Parser [Expression]
commaSeparatedExpressions left right =
    between (symbol left) (symbol right) $ expression `sepBy` symbol ","


listLiteral :: Parser [Expression]
listLiteral =
    commaSeparatedExpressions "[" "]"


-- TODO: #2 Tuples cannot have a single member.
tupleLiteral :: Parser [Expression]
tupleLiteral =
    commaSeparatedExpressions "(" ")"


function :: Parser Function
function = do
    bindingName <- identifier
    -- TODO: Support pattern matching
    parameters <- many identifier
    symbol "="
    BoundFunctionDefinition bindingName parameters <$> expression


functionApplication :: Parser Expression
functionApplication = do
    bindingName <- identifier
    FunctionApplication bindingName <$> many expression


anonymousFunction :: Parser Expression
anonymousFunction = do
    symbol "\\"
    parameters <- some $ identifier <|> symbol "_"
    symbol "->"
    AnonymousFunction parameters <$> expression


letBinding :: Parser Expression
letBinding = do
    symbolNewline "let"
    level <- L.indentLevel
    bindings <- someLetBindings level
    LetBinding bindings <$> expression


someLetBindings :: Pos -> Parser [Function]
someLetBindings requiredIndentation = do
    indentation <- L.indentLevel
    -- We must have an initial binding.
    binding <- function
    -- Bindings need to be separated with a newline but we need to give a
    -- chance for the `in` ending to be on the same line. The example below
    -- is valid Elm.
    --
    --     let
    --       x = 0
    --       y = 1 in x + y
    hasNewlineSeparator <- didConsume newline
    -- Adhere to the convention of consuming all trailing whitespace.
    optional space1
    done <- didConsume $ symbolNewline "in"
    case (done, hasNewlineSeparator, indentation == requiredIndentation) of
        (_, _, False) ->
            -- TODO: The error message for this will be slightly incorrect if
            -- this is the last binding before the `in` ending, since the error
            -- will point to the end of the expression.
            L.incorrectIndent EQ requiredIndentation indentation
        (True, _, _) ->
            return [binding]
        (False, False, _) ->
            fail "expected `in` or newline between let bindings"
        _ -> do
            bindings <- someLetBindings requiredIndentation
            return $ binding : bindings


caseExpression :: Parser Expression
caseExpression = do
    symbolNewline "case"
    subject <- expression
    optional space1
    symbolNewline "of"
    level <- L.indentLevel
    Cases subject <$> someCases level


someCases :: Pos -> Parser [Case]
someCases requiredIndentation = do
    indentation <- L.indentLevel
    -- TODO: Actual pattern syntax
    pattern <- expression
    symbolNewline "->"
    body <- expression
    hasNewlineSeparator <- didConsume newline
    optional space1
    case (hasNewlineSeparator, indentation == requiredIndentation) of
        (_, False) ->
            L.incorrectIndent EQ requiredIndentation indentation
        (True, True) -> do
            cases <- someCases requiredIndentation
            return $ (Case pattern body) : cases
        (False, True) -> do
            return [Case pattern body]


recordValue :: Parser Expression
recordValue =
    fmap (RecordValue . Map.fromList) $
        between (symbol "{") (symbol "}") $ recordMemberBinding `sepBy` symbol ","


recordUpdate :: Parser Expression
recordUpdate = do
    symbol "{"
    name <- identifier
    symbol "|"
    bindings <- recordMemberBinding `sepBy1` symbol ","
    symbol "}"
    return $ RecordUpdate name $ Map.fromList bindings


recordMemberBinding :: Parser (String, Expression)
recordMemberBinding = do
    key <- identifier
    symbol "="
    value <- expression
    return (key, value)


numberLiteral :: Parser Int
numberLiteral = lexeme L.decimal


floatLiteral :: Parser Float
floatLiteral = lexeme L.float


-- We should always use this instead of numberLiteral and floatLiteral because
-- they erroneously accept values such as `4.` or `4a` and return 4.
--
-- There might be a better way of doing this?
numberWrapper :: Parser Expression
numberWrapper = do
    -- Trailing floating points are not allowed in Elm. i.e., `4.`
    candidate <- getInput
    if '.' `elem` candidate
        then Float <$> floatLiteral
        else Int <$> numberLiteral


bool :: Parser Bool
bool =
    True <$ symbol "True" <|> False <$ symbol "False"


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


singleChar :: Parser Char
singleChar =
    surroundedBy (char '\'') $ escapedChar <|> noneOf "'\\"


singleLineString :: Parser String
singleLineString =
    surroundedBy (char '"') $
        many $ escapedChar <|> noneOf "\"\\\r\n"


multiLineString :: Parser String
multiLineString =
    surround >> manyTill (escapedChar <|> noneOf "\\") surround
  where
    surround = count 3 (char '"')
