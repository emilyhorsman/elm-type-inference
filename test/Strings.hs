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


letBindingTwoBindingsInvalidIndentationA = [r|let
    x = True
 y = True
in 0|]


letBindingTwoBindingsInvalidIndentationB = [r|let
    x = True
 y = True in 0|]


multiLineIfExpression = [r|if True
  then
    0
  else
    1|]


caseMultiplePatternsA = [r|case
foo
of
1 -> 1
2 -> 2|]


caseMultiplePatternsB = [r|case foo of
  1 ->
      1
  2 ->
      2|]


caseMultiplePatternsC = [r|case foo of 1 ->
    1
            2 -> 2|]


caseInvalidIndentation = [r|case foo of
1 -> 1
 2 -> 2|]
