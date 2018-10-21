{-# LANGUAGE QuasiQuotes #-}
module Strings where

import Text.RawString.QQ


multiLineIfExpression = [r|if True
  then
    0
  else
    1|]
