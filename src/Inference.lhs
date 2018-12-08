\documentclass{article}

\setlength\parindent{0pt}
\long\def\ignore#1{}

\usepackage{amsmath}
\usepackage{amssymb}
\usepackage[margin=0.75in]{geometry}

\usepackage{minted}
\newenvironment{code}{\VerbatimEnvironment\begin{minted}[
    linenos,
    firstnumber=last,
    mathescape,
    xleftmargin=0.5in,
    xrightmargin=0.5in,
    curlyquotes=true
    ]{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}

\begin{document}

\ignore{
\begin{code}
module Inference where

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

\begin{code}
unify :: Type -> Type -> Unifier
unify (TypeFunc left right) (TypeFunc left' right') =
    let
        leftUnifier =
            unify left left'
        rightUnifier =
            unify (applyUnifier leftUnifier right) (applyUnifier leftUnifier right')
    in
        composeUnifier rightUnifier leftUnifier

unify a@(TypeArg var) b
    | a == b = Map.empty
    | Set.member var (freeTypeVariables b) = error "oops"
    | otherwise = Map.singleton var b
unify b a@(TypeArg var)
    | a == b = Map.empty
    | Set.member var (freeTypeVariables b) = error "oops"
    | otherwise = Map.singleton var b

unify (Type constructor _) (Type constructor' _)
    | constructor /= constructor' = error "Cannot unify different constructors."
unify (Type constructor types) (Type constructor' types') =
    let
        params = zip types types'

        pairUnifier :: (Type, Type) -> Unifier
        pairUnifier =
            uncurry unify

        f :: Unifier -> (Type, Type) -> Unifier
        f unifier pair =
            composeUnifier unifier $ pairUnifier pair
    in
        foldl f Map.empty params

unify _ _ = error "unification failure"
\end{code}

\end{document}
