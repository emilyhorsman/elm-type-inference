module Inference where

import AST


expressionInference :: [TypeAlias] -> [TypeConstructorDefinition] -> Expression -> Type
expressionInference _ _ (Char _) =
    Type "Char" []

-- A String is not equivalent to List Char in Elm.
expressionInference _ _ (String _) =
    Type "String" []

expressionInference _ _ (Int _) =
    ConstrainedTypeVariable Number

expressionInference _ _ (Float _) =
    Type "Float" []

expressionInference _ _ (Bool _) =
    Type "Bool" []

expressionInference _ _ _ =
    Error
