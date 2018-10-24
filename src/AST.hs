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
    | RecordAccess String
    | QualifiedRecordAccess String String
    | QualifiedModuleAccess String String
    | BinOp BinOp Expression Expression
    | Negate Expression
    deriving (Show, Eq)


data CaseBranch
    = CaseBranch Pattern Expression
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
    | BooleanAnd
    | BooleanOr
    | Append
    | Cons
    | ApplyLeft
    | ApplyRight
    | ComposeLeft
    | ComposeRight
    deriving (Show, Eq)


data Pattern
    = PatternAnything
    | PatternVariable String
    | PatternString String
    | PatternChar Char
    | PatternInt Int
    | PatternFloat Float
    | PatternBool Bool
    | PatternList [Pattern]
    | PatternTuple [Pattern]
    | PatternCons Pattern Pattern
    | PatternRecord [String]
    | PatternAlias Pattern String
    deriving (Show, Eq)
