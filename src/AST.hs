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
    | Constructor String
    deriving (Show, Eq)


data CaseBranch
    = CaseBranch Pattern Expression
    deriving (Show, Eq)


data Declaration
    = BoundFunctionDefinition String [Pattern] Expression
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
    | PatternConstructor String [Pattern]
    deriving (Show, Eq)


data TypeAlias
    = TypeAlias String Type
    deriving (Show, Eq)


-- Elm's documentation calls this a ``CustomType'' and makes no reference to
-- ``type constructors`` but I reject this here because I don't find it clear
-- enough in this AST.
--
-- Elm used to call these ``union types``.
data TypeConstructorDefinition
    = TypeConstructorDefinition String [TypeConstructorArg] [Variant]
    deriving (Show, Eq)


data TypeConstructorArg
    = TypeConstructorArg String
    deriving (Show, Eq)


type VariantTag = String


-- Elm's documentation seems use the name ``variants'' instead of
-- ``data constructors''.
data Variant
    = Variant VariantTag [Type]
    deriving (Show, Eq)


-- This type should be accepted by both variants and signatures.
data Type
    = Type String [Type]
    | TypeArg String
    | TupleType [Type]
    | RecordType (Map.Map String Type)
    deriving (Show, Eq)
