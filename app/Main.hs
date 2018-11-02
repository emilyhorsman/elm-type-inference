module Main where

import System.Environment (getArgs)
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)

import Lib


main :: IO ()
main = do
    file <- head <$> getArgs
    source <- readFile file
    -- Modified from Text.Megaparsec (parseTest) to print error but pretty
    -- print AST.
    case parse topLevelProgram file source of
        Left e -> putStr (errorBundlePretty e)
        Right x -> pPrint x
