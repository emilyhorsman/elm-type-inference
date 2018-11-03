module Operators where

import Text.Megaparsec
import Text.Megaparsec.Char

import AST
import ParserDefinition
import Utility
import Whitespace


type OpP = Parser (Expression -> Expression -> Expression)


op n deterrent =
    (lexeme . try) (string n <* notFollowedBy deterrent)


operators :: [String]
operators =
    [ "<|"
    , "|>"
    , "<<"
    , ">>"
    , "++"
    , "::"
    , "&&"
    , "||"
    , "+"
    , "-"
    , "*"
    , "/="
    , "//"
    , "/"
    , "^"
    , "=="
    , "<="
    , "<"
    , ">="
    , ">"
    ]


operatorReference :: Parser String
operatorReference =
    choice $ string <$> operators


unaryNegative :: Parser (Expression -> Expression)
unaryNegative =
    -- Whitespace after unary minus operator is prohibited.
    Negate <$ op "-" space1


addOperator :: OpP
addOperator =
    BinOp Add <$ op "+" (char '+')
    -- ^ Using op because of the ++ operator conflicting.


minusOperator :: OpP
minusOperator =
    BinOp Minus <$ symbol "-"


multiplyOperator :: OpP
multiplyOperator =
    BinOp Multiply <$ symbol "*"


divideOperator :: OpP
divideOperator =
    BinOp Divide <$ symbol "/"


intDivideOperator :: OpP
intDivideOperator =
    BinOp IntegerDivide <$ symbol "//"


exponentiationOperator :: OpP
exponentiationOperator =
    BinOp Exponentiation <$ symbol "^"


equalityOperator :: OpP
equalityOperator =
    BinOp Equality <$ symbol "=="


noEqualityOperator :: OpP
noEqualityOperator =
    BinOp NoEquality <$ symbol "/="


ltOperator :: OpP
ltOperator =
    BinOp LessThan <$ op "<" (oneOf "|<")


lteOperator :: OpP
lteOperator =
    BinOp LessThanEq <$ symbol "<="


gtOperator :: OpP
gtOperator =
    BinOp GreaterThan <$ symbol ">"


gteOperator :: OpP
gteOperator =
    BinOp GreaterThanEq <$ symbol ">="


booleanAndOperator :: OpP
booleanAndOperator =
    BinOp BooleanAnd <$ symbol "&&"


booleanOrOperator :: OpP
booleanOrOperator =
    BinOp BooleanOr <$ symbol "||"


appendOperator :: OpP
appendOperator =
    BinOp Append <$ symbol "++"


consOperator :: OpP
consOperator =
    BinOp Cons <$ symbol "::"


applyLeftOperator :: OpP
applyLeftOperator =
    BinOp ApplyLeft <$ symbol "<|"


applyRightOperator :: OpP
applyRightOperator =
    BinOp ApplyRight <$ symbol "|>"


composeLeftOperator :: OpP
composeLeftOperator =
    BinOp ComposeLeft <$ symbol "<<"


composeRightOperator :: OpP
composeRightOperator =
    BinOp ComposeRight <$ symbol ">>"
