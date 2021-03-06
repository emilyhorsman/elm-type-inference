\documentclass{article}

\setlength\parindent{0pt}
\long\def\ignore#1{}

\usepackage{amsmath}
\usepackage{amssymb}
\usepackage[utf8]{inputenc}
\usepackage[top=1in,bottom=1in,left=1.5in,right=1.5in]{geometry}
\usepackage{url}

\usepackage{fontspec}
\setmonofont[
  Contextuals={Alternate}
]{Fira Code}

\usepackage{minted}
\setminted{
    mathescape,
    curlyquotes=true,
    fontsize=\footnotesize,
    breaklines=true,
    linenos=true,
    firstnumber=last
}
\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newcommand{\piercefootnote}[1]{\footnote{Types and Programming Languages, Pierce p. #1}}

\begin{document}

\definecolor{bg}{rgb}{0.95,0.95,0.95}

\ignore{
\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall #-}
module Inference where

import Control.Arrow (second)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AST
import Annotations
\end{code}
}

\section{Type Inference}

\subsection{Terminology}

\begin{description}
\item [Unifier]
    A type substitution $\sigma$ that maps type variables to types.
\item [Free type variable]
    A type variable in a type that is not bound by a quantifier.
    In the type $\forall\, \alpha : \alpha \to \beta$ the bound variables are $\{\alpha\}$ and the free variables are $\{\beta\}$.
\end{description}

\subsection{Type Substitutions (Unifiers)}

\begin{code}
type VariableName = String

type Unifier = Map.Map VariableName Type
\end{code}

If we have a unifier $\sigma = [X \mapsto \texttt{Bool}]$ then we can apply it to the type $T = X \to X$ as follows.

$$\sigma T = \sigma(X \to X) = \texttt{Bool} \to \texttt{Bool}$$

We'll need to apply a unifier's mapping on to types, type schemes, and environments.
We also require knowing the free type variables of each for avoiding infinite types, instantiation, and generalization.
It's worth creating a type class \texttt{Typing} for this.

\begin{code}
class Typing t where
    apply :: Unifier -> t -> t
    freeVariables :: t -> Set.Set VariableName

instance Typing Type where
    apply unifier t@(TypeArg x) =
        Map.findWithDefault t x unifier

    apply unifier (Type constructorName types) =
        Type constructorName $ apply unifier <$> types

    apply unifier (TypeFunc a b) =
        TypeFunc (apply unifier a) (apply unifier b)

    apply unifier (TupleType types) =
        TupleType $ apply unifier <$> types

    apply unifier (RecordType typeMap) =
        RecordType $ apply unifier <$> typeMap

    apply _ t@(ConstrainedTypeVariable _) =
        t

    freeVariables (TypeArg var) =
        Set.singleton var

    freeVariables (TypeFunc left right) =
        Set.union (freeVariables left) (freeVariables right)

    freeVariables (Type _ types) =
        Set.unions $ freeVariables <$> types

    freeVariables (TupleType types) =
        Set.unions $ freeVariables <$> types

    freeVariables (RecordType typeMap) =
        Set.unions $ freeVariables <$> typeMap

    freeVariables (ConstrainedTypeVariable _) =
        Set.empty
\end{code}

The unification algorithm requires a composition operator for unifiers.
Pierce defines it with the following set comprehension.\piercefootnote{318}

$$
\sigma \circ \gamma = \begin{cases}
    X \mapsto \sigma(T) & \forall\, (X \mapsto T) \in \gamma \label{1} \\
    X \mapsto T & \forall\, (X \mapsto T) \in \sigma \mid X \not\in \text{dom}(\gamma) \label{2}
\end{cases}
$$

That is, apply the unifier $\sigma$ to the mappings in $\gamma$ and then union the mappings in $\sigma$ which do not have a type variable in the domain of $\gamma$.

\begin{code}
composeUnifier :: Unifier -> Unifier -> Unifier
composeUnifier u1 u2 =
    let
        u2' =
            Map.map (apply u1) u2
    in
        -- From the Map.union docs: `takes the left biased union of t1 and t2.
        -- It prefers t1 when duplicate keys are encountered.'
        -- We want mappedGamma to be the left map since mappings in $\sigma$
        -- are excluded if their type variable is in the domain of $\gamma$.
        Map.union u2' u1
\end{code}

\subsection{Unification}

We want to ensure we do not produce an infinite type when assigning a type variable to a type.
Thus, there is no defined unifier for $a$ and $\texttt{List}\, a$ as $[a \mapsto \texttt{List}\, a]$ would produce an infinite type.
Unifying $b$ and $\texttt{List}\, a$ would be fine though, this is simply $[b \mapsto \texttt{List}\, a]$.

\begin{code}
infiniteTypeErrorMessage :: String
infiniteTypeErrorMessage =
    "Cannot unify an infinite type."

assignType :: Type -> Type -> Unifier
assignType t1@(TypeArg name) t2
    | t1 == t2 = Map.empty
    | Set.member name (freeVariables t2) = error infiniteTypeErrorMessage
    | otherwise = Map.singleton name t2
\end{code}

Now we can produce a unifier for two given types.

\begin{code}
differentConstructorsErrorMessage :: String
differentConstructorsErrorMessage =
    "Cannot unify different constructors."

unify :: Type -> Type -> Unifier
unify (TypeFunc left right) (TypeFunc left' right') =
    let
        leftUnifier =
            unify left left'
        rightUnifier =
            unify
                (apply leftUnifier right)
                (apply leftUnifier right')
    in
        composeUnifier rightUnifier leftUnifier

unify a@(TypeArg _) b =
    assignType a b
unify b a@(TypeArg _) =
    assignType a b

unify (Type constructor _) (Type constructor' _)
    | constructor /= constructor' = error differentConstructorsErrorMessage
unify (Type _ types) (Type _ types') =
    let
        pairUnifier :: (Type, Type) -> Unifier
        pairUnifier =
            uncurry unify

        f :: Unifier -> (Type, Type) -> Unifier
        f unifier pair =
            composeUnifier unifier $ pairUnifier pair

        params = zip types types'
    in
        foldl f Map.empty params

unify a b@(ConstrainedTypeVariable _) =
    unify b a
unify (ConstrainedTypeVariable Number) (Type "Float" []) =
    Map.empty

unify a b = error $ "Unification failure between `" ++ show a ++ "` and `" ++ show b ++ "`"
\end{code}

We can now achieve a result like the following.
See the \texttt{InferenceSpec} test suite for more.

$$\sigma(\alpha \to \texttt{Char}) = \sigma(\texttt{List}\, \beta \to \beta)$$
$$\sigma = [\alpha \mapsto \texttt{List Char}, \beta \mapsto \texttt{Char}]$$

\begin{minted}[escapeinside=||,bgcolor=bg]{haskell}
|$\lambda$|> let s = TypeArg "a" `TypeFunc` Type "Char" []
|$\lambda$|> let t = Type "List" [TypeArg "b"] `TypeFunc` TypeArg "b"
|$\lambda$|> s `unify` t
fromList [("a",Type "List" [Type "Char" []]),("b",Type "Char" [])]
\end{minted}

\subsection{Inference}

Implicit type annotations (see 22.6 of Pierce\piercefootnote{330}) requires generating fresh type variables.

\begin{code}
getFreshVarName :: Int -> String
getFreshVarName = (++) "t" . show

type TypeVariablesState = State Int

-- "t0", "t1", "t2", ...
fresh :: TypeVariablesState String
fresh = do
    s <- get
    put $ s + 1
    return $ getFreshVarName s
\end{code}

Inference time!

\begin{code}
data TypeScheme = TypeScheme
    { bound :: Set.Set String
    , body :: Type
    }
data Environment = Environment (Map.Map Expression TypeScheme)

removeKeys :: (Foldable t, Ord k) => Map.Map k a -> t k -> Map.Map k a
removeKeys m keys =
    foldr Map.delete m keys

instance Typing TypeScheme where
    apply unifier t@(TypeScheme {bound, body}) =
        t { body = apply (removeKeys unifier bound) body }

    freeVariables (TypeScheme {bound, body}) =
        Set.difference (freeVariables body) bound

instance Typing Environment where
    apply unifier (Environment gamma) =
        Environment $ Map.map (apply unifier) gamma

    freeVariables (Environment gamma) =
        Set.unions $ freeVariables <$> Map.elems gamma

-- Produce a fresh type variable for every bound type variable.
instantiate :: TypeScheme -> TypeVariablesState Type
instantiate (TypeScheme {bound, body}) = do
    let bound' = Set.elems bound
    freshTypeVariableNames <- mapM (const fresh) bound'
    let freshTypeVariables = TypeArg <$> freshTypeVariableNames
    let unifier = Map.fromList $ zip bound' freshTypeVariables
    return $ apply unifier body

generalize :: Environment -> Type -> TypeScheme
generalize gamma t = TypeScheme
    { bound = Set.difference (freeVariables t) (freeVariables gamma)
    , body = t
    }

assign :: Environment -> VariableName -> TypeScheme -> Environment
assign (Environment gamma) param scheme =
    Environment $ Map.insert (Variable param) scheme gamma
\end{code}

We also need some infrastructure for handling type constructors and their variants (also known as data constructors).

\begin{code}
type Defs = (Map.Map VariantTag (Variant, TypeConstructorDefinition))
data Definitions
    = Definitions Defs
    deriving (Show, Eq, Ord)

fromVariant :: TypeConstructorDefinition -> Variant -> Defs
fromVariant def v@(Variant tag _) =
    Map.singleton tag (v, def)

constructDefinitions :: [TypeConstructorDefinition] -> Definitions
constructDefinitions [] =
    Definitions $ Map.empty

constructDefinitions (def@(TypeConstructorDefinition _ _ variants) : defs) =
    let
        (Definitions defMap) = constructDefinitions defs
    in
        Definitions $ Map.union
            (Map.unions $ (fromVariant def) <$> variants)
            defMap
\end{code}

Take the following Elm custom type.

\begin{minted}[bgcolor=bg]{elm}
type Maybe a = Nothing | Just a
\end{minted}

The variants \texttt{Nothing} and \texttt{Just} require types.
Using either of them will return a \texttt{Maybe a}.
We must instantiate a fresh type variable for the type constructor parameters similarly to the instantiation we did for let-polymorphism.
In this case, we'll produce a mapping $a \mapsto t_i$ where $t_i$ is a fresh type variable.
\texttt{Nothing} will have type $\texttt{Maybe}\, t_i$ whereas a usage like \texttt{Just True} will have type \texttt{Maybe Bool}.
Instead of inferring \texttt{Just True} monolithically, we infer \texttt{Just} to be the function type $\alpha \to \texttt{Maybe}\,\alpha$ and then apply \texttt{True}.

\begin{code}
constructorType :: Map.Map String Type -> Type -> [Type] -> Type
constructorType _ resultType [] =
    resultType

constructorType typeVarMap resultType (Type tag children : args) =
    TypeFunc
        -- Variants can't be defined with a partial application of a type
        -- constructor so we don't want to produce a tree of TypeFunc instances
        -- here, we only want to map the usages of the type constructor
        -- arguments found in the variants.
        -- i.e., This is illegal since Either isn't fully applied:
        --     type Foo a = Bar (Either a)
        --
        -- The following requires deeply replacing the type constructor
        -- arguments `a` and `b` with their instantiations $t_i$ and $t_j$.
        --     type Foo a b = Bar (Either (Maybe a) (Maybe b))
        --
        -- Also using `apply` here is totally abusing the Unifier instance, lol.
        (Type tag $ apply typeVarMap <$> children)
        (constructorType typeVarMap resultType args)

-- TODO: We need to handle other types here.
constructorType typeVarMap resultType (TypeArg var : args) =
    case Map.lookup var typeVarMap of
        Nothing ->
            error $ "Type argument `" ++ var ++ "` used in variant is not in type constructor."

        Just t ->
            TypeFunc t (constructorType typeVarMap resultType args)

constructorType _ _ _ =
    error "Unimplemented constructor type!"
\end{code}

The variants in the type constructor definition don't matter, we are only instantiating fresh type variables for the type parameters of the type constructor, e.g., in \texttt{type Either a b} we have $[\texttt{a} \mapsto t_i, \texttt{b} \mapsto t_j]$.
This is being inferred from a given variant.

\begin{code}
instantiateConstructor :: (Variant, TypeConstructorDefinition) -> TypeVariablesState Type
instantiateConstructor (Variant _ types, TypeConstructorDefinition typeName typeArgs _) = do
    freshTypeVariableNames <- mapM (const fresh) typeArgs
    let freshTypeVariables = TypeArg <$> freshTypeVariableNames
    let resultType = Type typeName freshTypeVariables
    let typeVarMap = Map.fromList $ zip
            (map (\(TypeConstructorArg a) -> a) typeArgs)
            freshTypeVariables
    return $ constructorType typeVarMap resultType types
\end{code}

Pattern inference requires some plumbing functions.
Say we are inferring \mintinline{elm}{f (Just a) = a}.
\texttt{Just} will give us type $t_0 \to \texttt{Maybe}\,t_0$.
\texttt{a} will give us $t_1$ with the environment updated to include $[\texttt{a} \mapsto t_1]$.
We want to apply the type of \texttt{a} to the first parameter of \texttt{Just} so that the type of the pattern \texttt{Just a} is actually $\texttt{Maybe}\,t_0$.

\begin{code}
resolveApplication :: Type -> Type -> (Unifier, Type)
-- resolveApplication (TypeFunc $t_0$ (Maybe $t_0$)) (TypeArg $t_1$) = ($[t_1 \mapsto t_0]$, Maybe $t_0$)
resolveApplication (TypeFunc left right) callType =
    let
        unifier = unify callType left
    in
        (unifier, apply unifier right)

resolveApplication _ _ =
    error "Attempted to resolve application of non-function type."
\end{code}

\texttt{resolvePatterns} takes a partially applied data constructor type and a list of patterns and essentially `calls' the data constructor at the type level.

\begin{code}
resolvePatterns :: Definitions -> (Environment, Unifier, Type) -> [Pattern] -> TypeVariablesState (Environment, Unifier, Type)
resolvePatterns _ triple [] =
    return triple

resolvePatterns defs (gamma, unifier, partialConstructorType) (pattern : patterns) = do
    (gamma', patternType) <- inferPattern defs gamma pattern
    let (unifier', t) = resolveApplication partialConstructorType patternType
    let u = composeUnifier unifier unifier'
    resolvePatterns defs (apply u gamma', u, t) patterns

inferPattern :: Definitions -> Environment -> Pattern -> TypeVariablesState (Environment, Type)
inferPattern _ gamma PatternAnything =
    (gamma,) <$> TypeArg <$> fresh

inferPattern _ gamma@(Environment envMap) (PatternVariable param) =
    -- See ``does not re-instantiate type arguments with prior usage'' in
    -- InferenceSpec for a test that would fail if we did not check the
    -- environment before instantiating.
    case Map.lookup (Variable param) envMap of
        Nothing -> do
            freshTypeVariable <- TypeArg <$> fresh
            let scheme = TypeScheme { bound = Set.empty, body = freshTypeVariable }
            let gamma' = assign gamma param scheme
            return (gamma', freshTypeVariable)

        -- Ensure that a pattern like (a, a) would not re-instantiate the second a.
        Just (TypeScheme {body}) ->
            return (gamma, body)

inferPattern defs gamma (PatternTuple patterns) =
    let
        f :: (Environment, [Type]) -> Pattern -> TypeVariablesState (Environment, [Type])
        f (gamma', childrenTypes) pattern =
            second (flip (:) childrenTypes) <$> inferPattern defs gamma' pattern
    in
        second (TupleType . reverse) <$> foldM f (gamma, []) patterns
\end{code}

Inferring a constructor involves inferring the type of the variant (data constructor) and then applying its pattern arguments to it.
See ``infers a complex constructor pattern with multiple pattern arguments'' in \texttt{InferenceSpec}.

\begin{code}
inferPattern (Definitions defsMap) gamma (PatternConstructor tag []) =
    case Map.lookup tag defsMap of
        Nothing ->
            error $ "`" ++ tag ++ "` is not in definitions."

        Just pair ->
            (gamma,) <$> instantiateConstructor pair

inferPattern defs gamma (PatternConstructor tag patterns) = do
    (gamma', constructorT) <- inferPattern defs gamma (PatternConstructor tag [])
    (gamma'', unifier, t) <- resolvePatterns defs (gamma', Map.empty, constructorT) patterns
    return (gamma'', t)

inferPattern _ gamma pattern =
    let
        constructor = case pattern of
            PatternString _ -> "String"
            PatternChar _ -> "Char"
            PatternInt _ -> "Int"
            PatternFloat _ -> "Float"
            PatternBool _ -> "Bool"
            _ -> error "Missing pattern type in inferPattern"
    in
        return $ (gamma, Type constructor [])
\end{code}

Base types are trivial to infer.

\begin{code}
elmPrelude :: Map.Map Expression TypeScheme
elmPrelude =
    Map.map (generalize (Environment Map.empty)) basicAnnotations

infer :: Definitions -> Environment -> Expression -> TypeVariablesState (Unifier, Type)
-- TODO: Is the number handling here correct?
infer _ _ (Int _) =
    return (Map.empty, ConstrainedTypeVariable Number)

infer _ _ (Float _) =
    return (Map.empty, Type "Float" [])

-- TODO: comparable constrained type variable
infer _ _ (Char _) =
    return (Map.empty, Type "Char" [])

-- NOTE: A String is not equivalent to List Char in Elm.
-- TODO: appendable, compappend constrained type variables
infer _ _ (String _) =
    return (Map.empty, Type "String" [])

infer _ _ (Bool _) =
    return (Map.empty, Type "Bool" [])
\end{code}

We must instantiate the type scheme we get from the environment.
See Step 5 in Pierce\piercefootnote{334}.

\begin{code}
infer _ (Environment gamma) variable@(Variable name) =
    case Map.lookup variable (Map.union elmPrelude gamma) of
        Nothing ->
            error $ name ++ " is unbound."

        Just scheme -> do
            t <- instantiate scheme
            return (Map.empty, t)

infer defs gamma (AnonymousFunction [] expr) =
    infer defs gamma expr
\end{code}

We need to correct infer \texttt{f (Just 'a') = True} as $\texttt{Maybe Char} \to \texttt{Bool}$.
We'll need the same mechanism here when inferring \texttt{case} expressions.

\begin{code}
infer defs gamma (AnonymousFunction [pattern] expr) = do
    (gamma', patternType) <- inferPattern defs gamma pattern
    (unifier, exprType) <- infer defs gamma' expr
    return
        ( unifier
        , TypeFunc (apply unifier patternType) exprType
        )

infer defs gamma (AnonymousFunction (param : params) expr) = do
    infer defs gamma $
        AnonymousFunction
            [param]
            (AnonymousFunction params expr)
\end{code}

Function application is trickier.
We need to infer the function we're applying to (call this $t_1$) and then infer the argument we're applying (call this $t_2$).
The type of the result is $t_3$.
We want to produce a unifier $\sigma$ such that $\sigma(t_1) = \sigma(t_2 \to t_3)$.

\begin{code}
infer defs gamma (FunctionApplication expr1 expr2) = do
    (unifier1, type1) <- infer defs gamma expr1
    let gamma' = apply unifier1 gamma
    (unifier2, type2) <- infer defs gamma' expr2
    resultType <- TypeArg <$> fresh
    let curried = TypeFunc type2 resultType
    -- Whichever constraints we learned from inferring $t_2$ must be applied to
    -- $t_1$ since they have been independent until now.
    let unifier3 = unify (apply unifier2 type1) curried
    return $
        ( composeUnifier (composeUnifier unifier3 unifier2) unifier1
        , apply unifier3 resultType
        )
\end{code}

Let-polymorphism time!
Pierce describes a caveat we must solve in the inference algorithm.
Examine the following example code:

\begin{minted}[bgcolor=bg]{elm}
let
    id x = x
in
    let
        foo = id 1
    in
        id True
\end{minted}

The outer \texttt{let} binding describes an identity function with type $\forall\, \alpha : \alpha \to \alpha$.
There are two usages of the identity function.
One uses it with an integer argument and the other a boolean.
This is legal, but it requires that each usage gets a fresh type variable instead of sharing $\alpha$.
However, we only want to give usages a fresh type variable for the bound type variables.
This means that we must know which of the type variables in an assumption/environment $\Gamma$ are bound.
\\\\
See the ``handles let polymorphism'' test in \texttt{InferenceSpec} for an example of a unification between \texttt{Char} and \texttt{Bool} which would fail if the function types were not generalized and then used with fresh instantiations.

\begin{code}
infer defs gamma (LetBinding [BoundFunctionDefinition Nothing name patterns body] letExpr) = do
    (unifier1, bindingType) <- infer defs gamma (AnonymousFunction patterns body)
    let gamma' = apply unifier1 gamma
    let scheme = generalize gamma' bindingType
    let gamma'' = assign gamma' name scheme
    (unifier2, letBodyType) <- infer defs gamma'' letExpr
    return (composeUnifier unifier1 unifier2, letBodyType)

infer defs gamma (LetBinding (decl : decls) letExpr) =
    infer defs gamma $ LetBinding [decl] (LetBinding decls letExpr)
\end{code}

We assume the child type of a \texttt{List} type is inferred from its first member.

\begin{code}
-- TODO: comparable, compappend, appendable constrained type variables
infer _ _ (List []) = do
    typeArg <- TypeArg <$> fresh
    return (Map.empty, Type "List" [typeArg])

infer defs gamma (List (car : _)) = do
    (unifier, childType) <- infer defs gamma car
    return (unifier, Type "List" [childType])
\end{code}

\begin{code}
infer defs gamma (Tuple children) = do
    pairs <- mapM (infer defs gamma) children
    let unifier = foldr composeUnifier Map.empty $ fst <$> pairs
    return (unifier, TupleType $ snd <$> pairs)
\end{code}

A conditional is really just a function with a type signature $\mathbb{B} : \alpha \to \alpha \to \alpha$.
We're only worried about inference and not type checking.
This means we can simply infer the type $\alpha$ of one of the branches.

\begin{code}
infer defs gamma (If _ trueBranch _) =
    infer defs gamma trueBranch

-- TODO: The pattern could bind something which the branch expression requires
-- to infer the type.
infer defs gamma (Case _ ((CaseBranch _ branch) : _)) =
    infer defs gamma branch
\end{code}

Inferring the type of a data constructor/variant usage requires looking up the variant tag in the definitions map.
We then build a type from the variant using \texttt{instantiateConstructor}.

\begin{code}
infer (Definitions defs) _ (Constructor tag) =
    case Map.lookup tag defs of
        Nothing ->
            error $ "`" ++ tag ++ "` is not in definitions."

        Just pair -> do
            t <- instantiateConstructor pair
            return (Map.empty, t)
\end{code}

Record types are similar to tuples.
There is no support for row polymorphism with records yet.

\begin{code}
infer defs gamma (RecordValue recordMap) = do
    typeMap <- mapM (infer defs gamma) recordMap
    let unifier = foldr (composeUnifier . fst) mempty typeMap
    return (unifier, RecordType $ Map.map snd typeMap)

infer defs gamma (RecordUpdate name _) =
    -- TODO: Could the updated value refine the type?
    infer defs gamma (Variable name)
\end{code}

\begin{code}
infer defs gamma (Negate expr) =
    infer defs gamma (FunctionApplication (Variable "negate") expr)

infer defs gamma (BinOp op expr1 expr2) =
    let
        func = case op of
            Append ->
                "append"

            _ ->
                "always"
    in
        infer defs gamma $
            FunctionApplication
                (FunctionApplication (Variable func) expr1)
                expr2
\end{code}

\section{Resources}

\begin{enumerate}
    \item \url{https://cseweb.ucsd.edu/classes/wi14/cse230-a/lectures/lec-inference.html}
    \item \url{https://web.archive.org/web/20181114220414/http://dev.stephendiehl.com/fun/006_hindley_milner.html#worked-examples}
\end{enumerate}

\end{document}
