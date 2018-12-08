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

import AST
\end{code}
}

\section{Type Inference}

\subsection{Plumbing}

A type substitution $\sigma$ is a mapping from type variables to types.

\begin{code}
type TypeSubstitution = Map.Map String Type
\end{code}

If we have a substitution $\sigma = [X \mapsto \texttt{Bool}]$ then we can apply it to the type $T = X \to X$ as follows.

$$\sigma T = \sigma(X \to X) = \texttt{Bool} \to \texttt{Bool}$$

\begin{code}
applySubstitution :: TypeSubstitution -> Type -> Type
applySubstitution substitution t@(TypeArg x) =
    Map.findWithDefault t x substitution

applySubstitution substitution (Type constructorName types) =
    Type constructorName $ applySubstitution substitution <$> types

applySubstitution substitution (TypeFunc a b) =
    TypeFunc (applySubstitution substitution a) (applySubstitution substitution b)

applySubstitution _ _ =
    Error
\end{code}

The unification algorithm requires a composition operator for substitutions.
Pierce defines it with the following set comprehension.

$$
\sigma \circ \gamma = \begin{cases}
    X \mapsto \sigma(T) & \forall\, (X \mapsto T) \in \gamma \label{1} \\
    X \mapsto T & \forall\, (X \mapsto T) \in \sigma \mid X \not\in \text{dom}(\gamma) \label{2}
\end{cases}
$$

That is, apply the substitution $\sigma$ to the mappings in $\gamma$ and then union the mappings in $\sigma$ which do not have a type variable in the domain of $\gamma$.

\begin{code}
composeSubstitution :: TypeSubstitution -> TypeSubstitution -> TypeSubstitution
composeSubstitution sigma gamma =
    let
        mappedGamma =
            Map.map (applySubstitution sigma) gamma
    in
        -- From the Map.union docs: `takes the left biased union of t1 and t2.
        -- It prefers t1 when duplicate keys are encountered.'
        -- We want mappedGamma to be the left map since mappings in $\sigma$
        -- are excluded if their type variable is in the domain of $\gamma$.
        Map.union mappedGamma sigma
\end{code}

\subsection{Constraint Generation}

\subsection{Unification}

\end{document}
