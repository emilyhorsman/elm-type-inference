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

\begin{document}

\ignore{
\begin{code}
module Inference where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AST
\end{code}
}

\section{Type Inference}

\subsection{Type Substitutions (Unifiers)}

A type substitution or `unifier' $\sigma$ is a mapping from type variables to types.

\begin{code}
type Unifier = Map.Map String Type
\end{code}

If we have a unifier $\sigma = [X \mapsto \texttt{Bool}]$ then we can apply it to the type $T = X \to X$ as follows.

$$\sigma T = \sigma(X \to X) = \texttt{Bool} \to \texttt{Bool}$$

\begin{code}
applyUnifier :: Unifier -> Type -> Type
applyUnifier unifier t@(TypeArg x) =
    Map.findWithDefault t x unifier

applyUnifier unifier (Type constructorName types) =
    Type constructorName $ applyUnifier unifier <$> types

applyUnifier unifier (TypeFunc a b) =
    TypeFunc (applyUnifier unifier a) (applyUnifier unifier b)

-- TODO
applyUnifier _ _ =
    Error
\end{code}

The unification algorithm requires a composition operator for unifiers.
Pierce defines it with the following set comprehension.

$$
\sigma \circ \gamma = \begin{cases}
    X \mapsto \sigma(T) & \forall\, (X \mapsto T) \in \gamma \label{1} \\
    X \mapsto T & \forall\, (X \mapsto T) \in \sigma \mid X \not\in \text{dom}(\gamma) \label{2}
\end{cases}
$$

That is, apply the unifier $\sigma$ to the mappings in $\gamma$ and then union the mappings in $\sigma$ which do not have a type variable in the domain of $\gamma$.

\begin{code}
composeUnifier :: Unifier -> Unifier -> Unifier
composeUnifier sigma gamma =
    let
        mappedGamma =
            Map.map (applyUnifier sigma) gamma
    in
        -- From the Map.union docs: `takes the left biased union of t1 and t2.
        -- It prefers t1 when duplicate keys are encountered.'
        -- We want mappedGamma to be the left map since mappings in $\sigma$
        -- are excluded if their type variable is in the domain of $\gamma$.
        Map.union mappedGamma sigma
\end{code}

\subsection{Unification}

\begin{code}
freeTypeVariables :: Type -> Set.Set String
freeTypeVariables (TypeArg var) =
    Set.singleton var

freeTypeVariables (TypeFunc left right) =
    Set.union (freeTypeVariables left) (freeTypeVariables right)

freeTypeVariables (Type _ types) =
    Set.unions $ freeTypeVariables <$> types
\end{code}

We want to ensure we do not produce an infinite type when assigning a type variable to a type.
Thus, there is no defined unifier for $a$ and $\texttt{List}\, a$ as $[a \mapsto \texttt{List}\, a]$ would produce an infinite type.
Unifying $b$ and $\texttt{List}\, a$ would be fine though, this is simply $[b \mapsto \texttt{List}\, a]$.

\begin{code}
infiniteTypeErrorMessage =
    "Cannot unify an infinite type."

assignType :: Type -> Type -> Unifier
assignType t1@(TypeArg name) t2
    | t1 == t2 = Map.empty
    | Set.member name (freeTypeVariables t2) = error infiniteTypeErrorMessage
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
                (applyUnifier leftUnifier right)
                (applyUnifier leftUnifier right')
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

Implicit type annotations (see 22.6 of Pierce) requires generating fresh type variables.

\begin{code}
getFreshVarName :: Int -> String
getFreshVarName = (++) "t" . show

type TypeVariablesState = State Int

fresh :: TypeVariablesState String
fresh = do
    state <- get
    put $ state + 1
    return $ getFreshVarName state
\end{code}

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

\begin{code}
type Environment = Map.Map Expression Type

infer :: Environment -> Expression -> TypeVariablesState (Unifier, Type)
infer _ (Char _) =
    return (Map.empty, Type "Char" [])

-- NOTE: A String is not equivalent to List Char in Elm.
infer _ (String _) =
    return (Map.empty, Type "String" [])

infer _ (Bool _) =
    return (Map.empty, Type "Bool" [])

infer gamma variable@(Variable _) =
    return $ case Map.lookup variable gamma of
        Nothing ->
            (Map.empty, Error)

        Just t ->
            (Map.empty, t)

-- TODO: Apply currying for multiple parameters
infer gamma (AnonymousFunction [param] expr) = do
    freshTypeVariable <- TypeArg <$> fresh
    let gamma' = Map.insert (Variable param) freshTypeVariable gamma
    (unifier, t) <- infer gamma' expr
    return (unifier, TypeFunc (applyUnifier unifier freshTypeVariable) t)
\end{code}

Function application is trickier.
We need to infer the function we're applying to (call this $t_1$) and then infer the argument we're applying (call this $t_2$).
The type of the result is $t_3$.
We want to produce a unifier $\sigma$ such that $\sigma(t_1) = \sigma(t_2 \to t_3)$.

\begin{code}
infer gamma (FunctionApplication expr1 expr2) = do
    (unifier1, type1) <- infer gamma expr1
    let gamma' = Map.map (applyUnifier unifier1) gamma
    (unifier2, type2) <- infer gamma' expr2
    resultType <- TypeArg <$> fresh
    let curried = TypeFunc type2 resultType
    -- Whichever constraints we learned from inferring $t_2$ must be applied to
    -- $t_1$ since they have been independent until now.
    let unifier3 = unify (applyUnifier unifier2 type1) curried
    return $
        ( composeUnifier (composeUnifier unifier3 unifier2) unifier1
        , applyUnifier unifier3 resultType
        )
\end{code}

\end{document}
