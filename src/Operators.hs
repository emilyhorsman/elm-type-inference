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
    BinOpL Add <$ symbol "+"


minusOperator :: OpP
minusOperator =
    BinOpL Minus <$ symbol "-"


multiplyOperator :: OpP
multiplyOperator =
    BinOpL Multiply <$ symbol "*"


divideOperator :: OpP
divideOperator =
    BinOpL Divide <$ symbol "/"


intDivideOperator :: OpP
intDivideOperator =
    BinOpL IntegerDivide <$ symbol "//"
