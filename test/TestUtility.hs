module TestUtility where

import System.FilePath
import Test.Hspec.Megaparsec
import Text.Megaparsec

import ParserDefinition


getPath fileName =
    "test" </> "samples" </> fileName


testParserWithFile parser fileName result = do
    source <- readFile (getPath fileName)
    parse parser fileName source `shouldParse` result
