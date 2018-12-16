module Annotations where

import Text.Megaparsec
import Data.Maybe

import AST
import Lib
import qualified Elm.Basics as Basics


basicAnnotations :: [(String, Type)]
basicAnnotations =
    catMaybes $ map (parseMaybe declarationAnnotation) Basics.source
