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


letBindingTypeAnnotation = [r|let
    x : Bool -> Int
    x y = 1 in x True|]


multiLineIfExpression = [r|if True
  then
    0
  else
    1|]


caseMultiplePatternsA = [r|case
foo
of
1 -> 1
_ -> 2|]


caseMultiplePatternsB = [r|case foo of
  1 ->
      1
  _ ->
      2|]


caseMultiplePatternsC = [r|case foo of 1 ->
    1
            _ -> 2|]


caseInvalidIndentation = [r|case foo of
1 -> 1
 2 -> 2|]


caseNonTrivialPatterns = [r|case foo of
    x :: y :: [] ->
        (x, y)
    (x :: xs as list) ->
        (x, list)
    _ ->
        (foo, foo)|]


funcTypeUnaryAnnotation = [r|x : Int
x = 1|]


funcTypeAnnotation = [r|x : Int -> Int -> (Int, Int)
x _ _ = (1, 2)|]


funcTypeAnnotationNested = [r|x : (Int -> Int) -> Int
x _ = 1|]


funcTypeAnnotationArgs = [r|x : Maybe a
x = Nothing|]


topLevelProgramSimpleA = [r|
import Html exposing (..)


main = text "Hello"
|]


topLevelProgramSimpleB = [r|import Html exposing (..)

main = text "Hello"
|]


topLevelProgramSimpleC = [r|import Html exposing (..)

main =
    text "Hello"
|]


topLevelProgramMultipleImports = [r|import Html exposing (..)
import Browser

main =
    text "Hello"
|]
