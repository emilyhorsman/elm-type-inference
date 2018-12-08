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
    xrightmargin=0.5in
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

applySubstitution _ _ =
    Error
\end{code}

\end{document}
