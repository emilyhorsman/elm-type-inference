module Annotations where

import Control.Arrow
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.Megaparsec

import AST
import Lib
import qualified Elm.Basics as Basics


basicAnnotations :: Map.Map Expression Type
basicAnnotations =
    Map.fromList $
        map (first Variable) $ catMaybes $
            map (parseMaybe declarationAnnotation) Basics.source
