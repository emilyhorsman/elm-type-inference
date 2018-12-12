\documentclass{article}

\setlength\parindent{0pt}
\long\def\ignore#1{}

\usepackage{amsmath}
\usepackage{amssymb}
\usepackage[utf8]{inputenc}
\usepackage[top=1in,bottom=1in,left=1.5in,right=1.5in]{geometry}

\usepackage{minted}
\newenvironment{code}{\VerbatimEnvironment\begin{minted}[
    linenos,
    firstnumber=last,
    mathescape,
    curlyquotes=true
    ]{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newcommand{\piercefootnote}[1]{\footnote{Types and Programming Languages, Pierce p. #1}}

\begin{document}

\ignore{
\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Inference where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AST
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

    -- TODO
    apply _ _ =
        error "Not yet implemented!"

    freeVariables (TypeArg var) =
        Set.singleton var

    freeVariables (TypeFunc left right) =
        Set.union (freeVariables left) (freeVariables right)

    freeVariables (Type _ types) =
        Set.unions $ freeVariables <$> types

    -- TODO
    freeVariables _ =
        error "Not yet implemented!"
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
unify (Type constructor types) (Type constructor' types') =
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

unify _ _ = error "unification failure"
\end{code}

We can now achieve a result like the following.
See the \texttt{InferenceSpec} test suite for more.

$$\sigma(\alpha \to \texttt{Char}) = \sigma(\texttt{List}\, \beta \to \beta)$$
$$\sigma = [\alpha \mapsto \texttt{List Char}, \beta \mapsto \texttt{Char}]$$

\begin{minted}[escapeinside=||]{haskell}
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
    state <- get
    put $ state + 1
    return $ getFreshVarName state
\end{code}

Inference time!

\begin{code}
data TypeScheme = TypeScheme
    { bound :: Set.Set String
    , body :: Type
    }
data Environment = Environment (Map.Map Expression TypeScheme)

removeKeys map keys =
    foldr Map.delete map keys

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

infer :: Environment -> Expression -> TypeVariablesState (Unifier, Type)
infer _ (Char _) =
    return (Map.empty, Type "Char" [])

-- NOTE: A String is not equivalent to List Char in Elm.
infer _ (String _) =
    return (Map.empty, Type "String" [])

infer _ (Bool _) =
    return (Map.empty, Type "Bool" [])
\end{code}

We must instantiate the type scheme we get from the environment.
See Step 5 in Pierce\piercefootnote{334}.

\begin{code}
infer (Environment gamma) variable@(Variable name) =
    case Map.lookup variable gamma of
        Nothing ->
            error $ name ++ " is unbound."

        Just scheme -> do
            t <- instantiate scheme
            return (Map.empty, t)

infer gamma (AnonymousFunction [] expr) =
    infer gamma expr

-- TODO: Handle pattern matching. :sweat_smile:
infer gamma (AnonymousFunction [PatternVariable param] expr) = do
    freshTypeVariable <- TypeArg <$> fresh
    let scheme = TypeScheme { bound = Set.empty, body = freshTypeVariable }
    let gamma' = assign gamma param scheme
    (unifier, t) <- infer gamma' expr
    return (unifier, TypeFunc (apply unifier freshTypeVariable) t)

infer gamma (AnonymousFunction (param : params) expr) = do
    infer gamma $
        AnonymousFunction
            [param]
            (AnonymousFunction params expr)
\end{code}

Function application is trickier.
We need to infer the function we're applying to (call this $t_1$) and then infer the argument we're applying (call this $t_2$).
The type of the result is $t_3$.
We want to produce a unifier $\sigma$ such that $\sigma(t_1) = \sigma(t_2 \to t_3)$.

\begin{code}
infer gamma (FunctionApplication expr1 expr2) = do
    (unifier1, type1) <- infer gamma expr1
    let gamma' = apply unifier1 gamma
    (unifier2, type2) <- infer gamma' expr2
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
Examine the following code:

\begin{minted}{haskell}
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
The following test fails from attempting to unify \texttt{Char} and \texttt{Bool} if the function types are not generalized and then used with fresh instantiations.

\inputminted[
    firstline=194,
    lastline=220,
    autogobble=true,
    highlightlines={208,215},
    highlightcolor=yellow
]{haskell}{test/InferenceSpec.hs}

Highlighted in \colorbox{yellow}{yellow} are the multiple usages of \texttt{f} which require fresh instantiations.

\begin{code}
infer gamma (LetBinding [BoundFunctionDefinition Nothing name patterns body] letExpr) = do
    (unifier1, bindingType) <- infer gamma (AnonymousFunction patterns body)
    let gamma' = apply unifier1 gamma
    let scheme = generalize gamma' bindingType
    let gamma'' = assign gamma' name scheme
    (unifier2, letBodyType) <- infer gamma'' letExpr
    return (composeUnifier unifier1 unifier2, letBodyType)

infer gamma (LetBinding (decl : decls) letExpr) =
    infer gamma $ LetBinding [decl] (LetBinding decls letExpr)
\end{code}

We assume the child type of a \texttt{List} type is inferred from its first member.

\begin{code}
infer gamma (List []) = do
    typeArg <- TypeArg <$> fresh
    return (Map.empty, Type "List" [typeArg])

infer gamma (List (car : cdr)) = do
    (unifier, childType) <- infer gamma car
    return (unifier, Type "List" [childType])
\end{code}

\begin{code}
infer gamma (Tuple children) = do
    pairs <- mapM (infer gamma) children
    let unifier = foldr composeUnifier Map.empty $ fst <$> pairs
    return (unifier, TupleType $ snd <$> pairs)
\end{code}

\end{document}
