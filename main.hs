type Term = String

data Conditional = Conditional Term Term Term

-- TODO: Handle reserved words


-- readUnsigned
number =
    many1 digitChar


-- readFloat
float =
    (many1 digitChar) ++ (char '.') ++ (many1 digitChar)


bool = choice
    [ True <$ string "True"
    , False <$ string "False"
    ]


escapedDoubleQuote =
    '"' <$ string "\\\""


escapedSingleQote =
    '\'' <$ string "\\'"


singleQuotedString =
    between (char '"') (char '"') $ many $ noneOf "\n\"" <|> escapedDoubleQuote


multiLineString =
    between surround surround $ many $ (try escapedDoubleQuote <|> anyChar)
    where
        surround = string "\"\"\""


singleChar =
    between surround surround $ (try escapedSingleQuote <|> anyChar)
    where
        surround = char '\''


recordMember = do
    memberKey <- bindingName
    char '='
    memberValue <- term
    return (memberKey, memberValue)


recordValue =
    between (char '{') (char '}') $ sepBy recordMember (char ',')


recordUpdate =
    between (char '{') (char '}') $
        liftM2 (,) (bindingName <* char '|') (sepBy recordMember (char ','))


term = choice
    [ number
    , float
    , singleQuotedString
    , multiLineString
    , singleChar
    , recordValue
    , list
    , conditional
    ]


list =
    between (char '[') (char ']') $ sepBy term (char ',')


-- 1 :: []
-- 1 :: 2 :: []
-- 1 :: 2 :: [3]
cons = liftM2 (++) (sepBy term (string "::")) (string "::" *> list)


conditional = do
    string "if"
    predicate <- term
    string "then"
    expr1 <- term
    string "else"
    expr2 <- term
    return $ Conditional predicate expr1 expr2


-- Function (possibly nullary) name or let binding name
-- Should support special cases like +, ++?
bindingName =
    alphaChar ++ (many alphaNumChar)


-- TODO: This is an incorrect simplification.
typeValue = choices
    [ bindingName
    , recordTypeValue
    , tupleTypeValue
    , listTypeValue
    ]


listTypeValue =
    between (char '[') (char ']') typeAnnotation


tupleTypeValue =
    between (char '(') (char ')') (sepBy typeAnnotation (char ','))


recordTypeValue =
    between (char '{') (char '}') (sepBy typeAnnotation (char ','))


-- TODO: type annotation can be support type -> type
-- type must support record types { a : Float b, : Float }
typeAnnotation = do
    name <- bindingName
    char ':'
    annotation <- sepBy typeValue (string "->")
    -- TODO: This needs a type, actually maybe it doesn't because we could have
    -- a dictionary built from a list of tuples
    return (name, annotation)


functionDefinition = do
    name <- bindingName
    char ' '
    -- TODO: Pattern matching support
    args <- (sepBy bindingName (char ' '))
    return (name, args)


tupleValue =
    between (char '(') (char ')') (sepBy term (char ','))


{-
TODO:
* case (context-sensitive grammar)
* Pattern matching on tuples, records, lists
    * Pattern matching is gonna be fun to do inference on…
* Function application
* Infix function application
* Built-in unary (-) and binary operators
* Field access
* Modules
-}
