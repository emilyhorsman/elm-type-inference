module AST where

import qualified Data.Map.Strict as Map


data Program
    = Program (Maybe ModuleStatement) [ProgramBody]
    deriving (Show, Eq)


data ProgramBody
    = ImportDecl ImportStatement
    | FunctionDecl Declaration
    | TypeAliasDecl TypeAlias
    | TypeDecl TypeConstructorDefinition
    deriving (Show, Eq)


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
    | AnonymousFunction [Pattern] Expression
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
    deriving (Show, Eq, Ord)


data CaseBranch
    = CaseBranch Pattern Expression
    deriving (Show, Eq, Ord)


data Declaration
    = BoundFunctionDefinition (Maybe Type) String [Pattern] Expression
    deriving (Show, Eq, Ord)


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
    deriving (Show, Eq, Ord)


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
    deriving (Show, Eq, Ord)


data TypeAlias
    = TypeAlias String Type
    deriving (Show, Eq, Ord)


-- Elm's documentation calls this a ``CustomType'' and makes no reference to
-- ``type constructors`` but I reject this here because I don't find it clear
-- enough in this AST.
--
-- Elm used to call these ``union types``.
data TypeConstructorDefinition
    = TypeConstructorDefinition String [TypeConstructorArg] [Variant]
    deriving (Show, Eq, Ord)


data TypeConstructorArg
    = TypeConstructorArg String
    deriving (Show, Eq, Ord)


type VariantTag = String


-- Elm's documentation seems use the name ``variants'' instead of
-- ``data constructors''.
data Variant
    = Variant VariantTag [Type]
    deriving (Show, Eq, Ord)


-- This type should be accepted by both variants and signatures.
data Type
    = Type String [Type]
    | TypeArg String
    | TypeFunc Type Type
    | TupleType [Type]
    | RecordType (Map.Map String Type)
    | ConstrainedTypeVariable ConstrainedTypeVariable
    | Error
    deriving (Show, Eq, Ord)


-- TODO: This is probably not the best method for handling Elm's four
-- constrained type variables.
data ConstrainedTypeVariable
    = Number
    | Appendable
    | Comparable
    | Compappend
    deriving (Show, Eq, Ord)


data ModuleStatement
    = ModuleStatement String [ModuleSymbol]
    deriving (Show, Eq)


data ImportStatement
    = ImportStatement String [ModuleSymbol]
    | QualifiedImportStatement String String [ModuleSymbol]
    deriving (Show, Eq)


data ModuleSymbol
    = AllSymbols
    | ModuleFunction String
    | ModuleType String [TypeSymbol]
    deriving (Show, Eq)


data TypeSymbol
    = AllVariants
    | TypeSymbol String
    deriving (Show, Eq)
