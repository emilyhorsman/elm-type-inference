module Utility where

import Text.Megaparsec


-- Modified pattern from Text.Megaparsec (atEnd)
didConsume p = option False $ True <$ p
