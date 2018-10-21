{-# LANGUAGE QuasiQuotes #-}
module Strings where

import Text.RawString.QQ


letBindingTwoBindingsA = [r|let
  x = True
  y = True
in 0|]


letBindingTwoBindingsB = [r|let x = True
    y = True
in 0|]


letBindingTwoBindingsC = [r|let x = True
    y = True in 0|]


letBindingTwoBindingsInvalidIndentation = [r|let
    x = True
 y = True
in 0|]


multiLineIfExpression = [r|if True
  then
    0
  else
    1|]
