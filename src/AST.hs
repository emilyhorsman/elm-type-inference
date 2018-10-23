module AST where

import qualified Data.Map.Strict as Map


data Expression
    = Char Char
    | String String
    | Int Int
    | Float Float
    | Bool Bool
    | List [Expression]
    | Tuple [Expression]
    | If Expression Expression Expression
    | FunctionApplication Expression Expression
    | AnonymousFunction [String] Expression
    | LetBinding [Declaration] Expression
    | Case Expression [CaseBranch]
    | RecordValue (Map.Map String Expression)
    | RecordUpdate String (Map.Map String Expression)
    | Variable String
    | BinOp BinOp Expression Expression
    deriving (Show, Eq)


data CaseBranch
    = CaseBranch Expression Expression
    deriving (Show, Eq)


data Declaration
    = BoundFunctionDefinition String [String] Expression
    deriving (Show, Eq)


data BinOp
    = Add
    | Minus
    | Multiply
    | Divide
    | IntegerDivide
    | Exponentiation
    | Equality
    | NoEquality
    | LessThan
    | LessThanEq
    | GreaterThan
    | GreaterThanEq
    deriving (Show, Eq)
