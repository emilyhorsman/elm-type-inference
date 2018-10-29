module Lib where

import Control.Monad (liftM2)
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
        , Bool <$> boolLiteral
        , Char <$> charLiteral
        , String <$> singleLineStringLiteral
        , String <$> multiLineStringLiteral
        , try $ symbol "(" *> expression <* symbol ")"
        , tupleExpression
        , listExpression
        ]


-- Based on Basics.elm
table =
    [ [ Prefix unaryNegative
      ]
    , [ InfixL functionApplicationJuxtaposition
      ]
    , [ InfixL composeLeftOperator
      , InfixR composeRightOperator
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
    , [ InfixR appendOperator
      , InfixR consOperator
      ]
    , [ InfixN equalityOperator
      , InfixN noEqualityOperator
      , InfixN ltOperator
      , InfixN lteOperator
      , InfixN gtOperator
      , InfixN gteOperator
      ]
    , [ InfixR booleanAndOperator
      ]
    , [ InfixR booleanOrOperator
      ]
    , [ InfixR applyLeftOperator
      , InfixL applyRightOperator
      ]
    ]


expression :: Parser Expression
expression =
    makeExprParser term table


variable :: Parser Expression
variable =
    choice
        [ try $ fmap Variable $ symbol "(" *> operatorReference <* symbol ")"
        -- Assumption here: an accessor beginning with an upper case character
        -- is a module. e.g., List.map
        , try $ liftM2 QualifiedModuleAccess moduleName dottedIdentifier
        , try $ liftM2 QualifiedRecordAccess identifier dottedIdentifier
        , RecordAccess <$> dottedIdentifier
        , Variable <$> identifier
        ]
  where
    dottedIdentifier =
        char '.' *> identifier

    moduleName =
        (:) <$> upperChar <*> many identifierChar


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


-- Generic list construction for listExpression and patternList
list :: ([a] -> b) -> Parser a -> Parser b
list dataConstructor parser =
    fmap dataConstructor $
        between (symbol "[") (symbol "]") $
            parser `sepBy` symbol ","


listExpression :: Parser Expression
listExpression =
    list List expression


-- Generic tuple construction for tupleExpression and patternTuple
tuple :: ([a] -> b) -> Parser a -> Parser b
tuple dataConstructor parser = do
    symbol "("
    a <- parser
    rest <- count' 1 2 $ symbol "," *> parser
    symbol ")"
    return $ dataConstructor (a : rest)


tupleExpression :: Parser Expression
tupleExpression =
    tuple Tuple expression


function :: Parser Declaration
function = do
    bindingName <- identifier
    -- `func x :: xs = …` is not valid Elm like `case x of x :: xs -> …` is.
    -- The cons operator must be surrounded in parenthesis when used outside a
    -- case branch.
    parameters <- many (try patternCons <|> patternTerm)
    symbol "="
    BoundFunctionDefinition bindingName parameters <$> expression
  where
    patternCons = do
        lhs <- pattern
        symbol "::"
        PatternCons lhs <$> pattern


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
    pat <- pattern
    symbolNewline "->"
    body <- expression
    hasNewlineSeparator <- didConsume newline
    optional space1
    case (hasNewlineSeparator, indentation == requiredIndentation) of
        (_, False) ->
            L.incorrectIndent EQ requiredIndentation indentation
        (True, True) -> do
            cases <- caseBranches requiredIndentation
            return $ (CaseBranch pat body) : cases
        (False, True) -> do
            return [CaseBranch pat body]


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


pattern :: Parser Pattern
pattern = makeExprParser patternTerm
    [ [ InfixR (PatternCons <$ symbol "::")
      ]
    ]


patternTerm :: Parser Pattern
patternTerm =
    choice
        [ anything
        , numberPattern
        , PatternBool <$> boolLiteral
        , PatternChar <$> charLiteral
        , PatternString <$> singleLineStringLiteral
        , PatternVariable <$> identifier
        , try patternAlias
        , try $ symbol "(" *> pattern <* symbol ")"
        , patternRecord
        , patternTuple
        , patternList
        ]


anything :: Parser Pattern
anything =
    PatternAnything <$ symbol "_"


patternAlias :: Parser Pattern
patternAlias = do
    symbol "("
    pat <- pattern
    symbol "as"
    PatternAlias pat <$> identifier <* symbol ")"


patternRecord :: Parser Pattern
patternRecord =
    fmap PatternRecord $
        between (symbol "{") (symbol "}") $ identifier `sepBy` symbol ","


patternTuple :: Parser Pattern
patternTuple =
    tuple PatternTuple pattern


patternList :: Parser Pattern
patternList =
    list PatternList pattern


constructorName :: Parser String
constructorName =
    lexeme $ (:) <$> upperChar <*> many alphaNumChar


typeConstructorDefinition :: Parser TypeConstructorDefinition
typeConstructorDefinition = do
    symbol "type"
    name <- constructorName
    arguments <- many $ TypeConstructorArg <$> identifier
    symbol "="
    variants <- variant `sepBy1` (symbol "|")
    return $ TypeConstructorDefinition name arguments variants


variant :: Parser Variant
variant = do
    tag <- constructorName
    Variant tag <$> many typeParser


typeParser :: Parser Type
typeParser =
    choice
        [ typeP
        , typeArgument
        , tupleType
        , recordType
        ]


typeP :: Parser Type
typeP =
    choice
        [ try precedence
        , noPrecedence
        ]
  where
    noPrecedence = do
        name <- constructorName
        return $ Type name []

    precedence = do
        symbol "("
        name <- constructorName
        args <- many typeParser
        symbol ")"
        return $ Type name args


typeArgument :: Parser Type
typeArgument =
    TypeArg <$> identifier


tupleType :: Parser Type
tupleType =
    tuple TupleType typeParser


recordType :: Parser Type
recordType =
    fmap (RecordType . Map.fromList) $
        between (symbol "{") (symbol "}") $ recordMemberTypeAnnotation `sepBy` symbol ","


recordMemberTypeAnnotation :: Parser (String, Type)
recordMemberTypeAnnotation = do
    key <- identifier
    symbol ":"
    annotation <- typeParser
    return (key, annotation)


typeAlias :: Parser TypeAlias
typeAlias = do
    symbol "type alias"
    name <- constructorName
    symbol "="
    TypeAlias name <$> typeParser
