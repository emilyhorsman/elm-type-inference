module Lib where

import Control.Monad.Combinators.Expr
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

import AST
import Elm
import Literals
import Operators
import ParserDefinition
import Utility
import Whitespace


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


term :: Parser Expression
term =
    choice
        [ anonymousFunction
        , letBinding
        , ifExpression
        , caseExpression
        , variable
        , numberLiteral
        , boolLiteral
        , charLiteral
        , singleLineStringLiteral
        , multiLineStringLiteral
        , try $ symbol "(" *> expression <* symbol ")"
        , tupleExpression
        , listExpression
        ]


table =
    [ [ InfixL functionApplicationJuxtaposition
      ]
    , [ InfixR exponentiationOperator
      ]
    , [ InfixL multiplyOperator
      , InfixL divideOperator
      , InfixL intDivideOperator
      ]
    , [ InfixL addOperator
      , InfixL minusOperator
      ]
    , [ InfixN equalityOperator
      , InfixN noEqualityOperator
      , InfixN ltOperator
      , InfixN lteOperator
      , InfixN gtOperator
      , InfixN gteOperator
      ]
    , [ InfixR booleanAndOperator
      , InfixR booleanOrOperator
      ]
    ]


expression :: Parser Expression
expression =
    makeExprParser term table


variable :: Parser Expression
variable =
    Variable <$> identifier


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


listExpression :: Parser Expression
listExpression =
    fmap List $ commaSeparatedExpressions "[" "]"


tupleExpression :: Parser Expression
tupleExpression = do
    symbol "("
    a <- expression
    rest <- count' 1 2 $ symbol "," >> expression
    symbol ")"
    return $ Tuple (a : rest)


function :: Parser Declaration
function = do
    bindingName <- identifier
    -- TODO: Support pattern matching
    parameters <- many identifier
    symbol "="
    BoundFunctionDefinition bindingName parameters <$> expression


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


someLetBindings :: Pos -> Parser [Declaration]
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
    Case subject <$> caseBranches level


caseBranches :: Pos -> Parser [CaseBranch]
caseBranches requiredIndentation = do
    indentation <- L.indentLevel
    -- TODO: Actual pattern syntax
    pattern <- choice
        [ variable
        , numberLiteral
        , boolLiteral
        , charLiteral
        , singleLineStringLiteral
        , multiLineStringLiteral
        ]
    symbolNewline "->"
    body <- expression
    hasNewlineSeparator <- didConsume newline
    optional space1
    case (hasNewlineSeparator, indentation == requiredIndentation) of
        (_, False) ->
            L.incorrectIndent EQ requiredIndentation indentation
        (True, True) -> do
            cases <- caseBranches requiredIndentation
            return $ (CaseBranch pattern body) : cases
        (False, True) -> do
            return [CaseBranch pattern body]


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
