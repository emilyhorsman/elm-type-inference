module Lib where

import Control.Monad (liftM2)
import Control.Monad.Combinators.Expr
import Data.List (intercalate)
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


functionApplicationJuxtaposition :: Pos -> OpP
functionApplicationJuxtaposition reference =
    FunctionApplication <$ try (symbolNewline "" <* lookAhead p)
  where
    -- Function application by juxtaposition means we need to disambiguate a
    -- situation like the following:
    --
    --     let
    --         x = func
    --           arg1
    --           arg2
    --         y = 1
    --     in …
    --
    -- Which should be (func arg1 arg2) but would be parsed as
    -- (func arg1 arg2 y) without checking indentation.
    p = do
        i <- L.indentLevel
        if i > reference
           then expression
           else L.incorrectIndent GT reference i


topLevelProgram :: Parser Program
topLevelProgram = do
    spaceConsumer space1
    maybeModuleStatement <- optional $ try $
        nonIndented moduleStatement <* newline
    spaceConsumer space1
    declarations <- statements `sepEndBy` some (newline <* space)
    eof
    return $ Program maybeModuleStatement declarations
  where
    statements =
        choice
            [ Left <$> nonIndented importStatement
            , Right <$> nonIndented function <?> "function definition"
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


term :: Parser Expression
term =
    choice
        [ anonymousFunction
        , letBinding
        , ifExpression
        , caseExpression
        , numberLiteral
        , Bool <$> boolLiteral
        , Char <$> charLiteral
        , String <$> singleLineStringLiteral
        , String <$> multiLineStringLiteral
        , variable
        , try $ symbol "(" *> expression <* symbol ")"
        , tupleExpression
        , listExpression
        , recordValue
        ]


-- Based on Basics.elm
table reference =
    [ [ Prefix unaryNegative
      ]
    , [ InfixL $ functionApplicationJuxtaposition reference
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
    contextualExpression pos1


contextualExpression :: Pos -> Parser Expression
contextualExpression reference =
    makeExprParser term $ table reference


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
        , Constructor <$> constructorName
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
        -- Consume newlines after opening bracket, comma separators, and members.
        -- Do not consume newlines after the closing bracket.
        between (symbolNewline "[") (symbol "]") $
            (parser <* spaceConsumer space1) `sepBy` symbolNewline ","


listExpression :: Parser Expression
listExpression =
    list List expression


-- Generic tuple construction for tupleExpression and patternTuple
tuple :: ([a] -> b) -> Parser a -> Parser b
tuple dataConstructor parser = do
    -- Consume newlines after opening paren, comma separators, and members.
    -- Do not consume newlines after the closing paren.
    symbolNewline "("
    a <- parser <* spaceConsumer space1
    rest <- count' 1 2 $ symbolNewline "," *> parser <* spaceConsumer space1
    symbol ")"
    return $ dataConstructor (a : rest)


tupleExpression :: Parser Expression
tupleExpression =
    tuple Tuple expression


function :: Parser Declaration
function = do
    maybeAnnotation <- optional $ try declarationAnnotation
    indentation <- L.indentLevel
    bindingName <- identifier
    -- `func x :: xs = …` is not valid Elm like `case x of x :: xs -> …` is.
    -- The cons operator must be surrounded in parenthesis when used outside a
    -- case branch.
    parameters <- many (try patternCons <|> patternTerm)
    symbolNewline "="
    let funcDef a = BoundFunctionDefinition a bindingName parameters
    case maybeAnnotation of
        Just (annotationBindingName, tree) ->
            if annotationBindingName == bindingName then
                funcDef (Just tree) <$> contextualExpression indentation
            else
                fail $ "`" ++ annotationBindingName ++ "` annotation must be followed by definition."

        Nothing ->
            funcDef Nothing <$> contextualExpression indentation
  where
    patternCons = do
        lhs <- pattern
        symbol "::"
        PatternCons lhs <$> pattern


declarationAnnotation :: Parser (String, Annotation)
declarationAnnotation = do
    bindingName <- identifier
    symbol ":"
    tree <- annotation
    space
    return $ (bindingName, tree)


annotation :: Parser Annotation
annotation =
    makeExprParser annotationTerm
        [ [ InfixR (BinAnnotation <$ symbol "->")
          ]
        ]

annotationTerm :: Parser Annotation
annotationTerm =
    choice
        [ try $ symbol "(" *> annotation <* symbol ")"
        , Annotation <$> typeParser True
        ]


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
        -- Consume newlines after opening brace, comma separators, and members.
        -- Do not consume newlines after the closing brace.
        between (symbolNewline "{") (symbol "}") $
            recordMemberBinding `sepBy` symbolNewline ","


recordUpdate :: Parser Expression
recordUpdate = do
    -- Consume newlines after opening brace, comma separators, pipe delimiter,
    -- and members.  Do not consume newlines after the closing brace.
    symbolNewline "{"
    name <- identifier <* spaceConsumer space1
    symbol "|" <* spaceConsumer space1
    bindings <- recordMemberBinding `sepBy1` symbolNewline ","
    symbol "}"
    return $ RecordUpdate name $ Map.fromList bindings


recordMemberBinding :: Parser (String, Expression)
recordMemberBinding = do
    key <- identifier <* spaceConsumer space1
    symbolNewline "="
    value <- expression <* spaceConsumer space1
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
        , try patternConstructor
        , try $ symbol "(" *> pattern <* symbol ")"
        , patternRecord
        , patternTuple
        , patternList
        ]


anything :: Parser Pattern
anything =
    PatternAnything <$ symbol "_"


patternConstructor :: Parser Pattern
patternConstructor =
    choice
        [ try precedence
        , noPrecedence
        ]
  where
    noPrecedence = do
        name <- constructorName
        return $ PatternConstructor name []

    precedence = do
        symbol "("
        name <- constructorName
        args <- many pattern
        symbol ")"
        return $ PatternConstructor name args


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
    Variant tag <$> many (typeParser False)


typeParser :: Bool -> Parser Type
typeParser inAnnotation =
    choice
        [ if inAnnotation then typePInAnnotation else typeP
        , typeArgument
        , tupleType
        , recordType
        ]


-- This version is used in function annotations where we have a `->` separator
-- and thus do not need parens for precedence in type arguments. There's
-- probably a better way to do this.
typePInAnnotation :: Parser Type
typePInAnnotation = do
    name <- constructorName
    Type name <$> many (typeParser False)


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
        args <- many $ typeParser False
        symbol ")"
        return $ Type name args


typeArgument :: Parser Type
typeArgument =
    TypeArg <$> identifier


tupleType :: Parser Type
tupleType =
    tuple TupleType $ typeParser False


recordType :: Parser Type
recordType =
    fmap (RecordType . Map.fromList) $
        between (symbol "{") (symbol "}") $ recordMemberTypeAnnotation `sepBy` symbol ","


recordMemberTypeAnnotation :: Parser (String, Type)
recordMemberTypeAnnotation = do
    key <- identifier
    symbol ":"
    annotation <- typeParser False
    return (key, annotation)


typeAlias :: Parser TypeAlias
typeAlias = do
    symbol "type alias"
    name <- constructorName
    symbol "="
    TypeAlias name <$> typeParser False


moduleName = constructorName


moduleStatement :: Parser ModuleStatement
moduleStatement = do
    symbol "module"
    name <- moduleName
    symbol "exposing"
    ModuleStatement name <$> choice
        [ [AllSymbols] <$ try (symbol "(..)")
        , moduleSymbols
        ]


moduleSymbols :: Parser [ModuleSymbol]
moduleSymbols =
    symbol "(" *> p `sepBy` symbol "," <* symbol ")"
  where
    p =
        choice
            [ ModuleFunction <$> identifier
            , try $ ModuleType <$> constructorName <*> typeSymbols
            -- ^ typeSymbols is intentionally not optional because we don't
            -- want just any failure to provide an empty [].
            , ModuleType <$> constructorName <*> return []
            ]


typeSymbols :: Parser [TypeSymbol]
typeSymbols =
    choice
        [ [AllVariants] <$ try (symbol "(..)")
        , symbol "(" *> sym <* symbol ")"
        ]
  where
    sym = (TypeSymbol <$> constructorName) `sepBy` symbol ", "


importStatement :: Parser ImportStatement
importStatement = do
    symbol "import"
    name <- intercalate "." <$> moduleName `sepBy1` char '.'
    choice
        [ try $ QualifiedImportStatement name
            <$> (symbol "as" *> moduleName) <*> unqualifiedImport
        , ImportStatement name <$> unqualifiedImport
        ]
  where
    unqualifiedImport =
        choice
            [ [] <$ lookAhead eof
            , [] <$ lookAhead newline
            , symbol "exposing" *> syms
            ]

    syms =
        choice
            [ [AllSymbols] <$ try (symbol "(..)")
            , moduleSymbols
            ]
