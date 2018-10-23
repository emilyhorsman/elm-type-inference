module Operators where

import AST
import ParserDefinition
import Whitespace


type OpP = Parser (Expression -> Expression -> Expression)


functionApplicationJuxtaposition :: OpP
functionApplicationJuxtaposition =
    FunctionApplication <$ symbol ""


addOperator :: OpP
addOperator =
    BinOp Add <$ symbol "+"


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
