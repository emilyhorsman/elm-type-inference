module TestUtility where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import System.FilePath
import Test.Hspec.Megaparsec
import Text.Megaparsec

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


parseInfer expr =
    case (parse expression "" expr) of
        Left _ ->
            error "Could not parse."

        Right e ->
            snd (evalState (infer emptyEnvironment e) 0)
