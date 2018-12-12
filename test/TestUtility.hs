module TestUtility where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import System.FilePath
import Test.Hspec.Megaparsec
import Text.Megaparsec

import AST
import Inference
import Lib
import ParserDefinition


getPath fileName =
    "test" </> "samples" </> fileName


testParserWithFile parser fileName result = do
    source <- readFile (getPath fileName)
    parse parser fileName source `shouldParse` result


emptyEnvironment :: Environment
emptyEnvironment = Environment $ Map.empty


emptyDefinitions :: Definitions
emptyDefinitions = Definitions $ Map.empty

nothing =
    Variant "Nothing" []

just =
    Variant "Just" [TypeArg "a"]

maybeDef =
    TypeConstructorDefinition
        "Maybe"
        [TypeConstructorArg "a"]
        [nothing, just]

left =
    Variant "Left" [TypeArg "a"]

right =
    Variant "Right" [TypeArg "b"]

eitherDef =
    TypeConstructorDefinition
        "Either"
        [TypeConstructorArg "a", TypeConstructorArg "b"]
        [left, right]


standardDefinitions :: Definitions
standardDefinitions =
    constructDefinitions
        [ maybeDef
        , eitherDef
        ]


parseInfer expr =
    case (parse expression "" expr) of
        Left _ ->
            error "Could not parse."

        Right e ->
            snd (evalState (infer emptyDefinitions emptyEnvironment e) 0)
