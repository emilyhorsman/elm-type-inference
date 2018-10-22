module Elm where

import qualified Data.Set as Set


reservedWords :: Set.Set String
reservedWords =
    Set.fromList
        [ "as"
        , "case"
        , "else"
        , "exposing"
        , "if"
        , "import"
        , "in"
        , "let"
        , "module"
        , "of"
        , "port"
        , "then"
        , "type"
        , "where"
        ]
