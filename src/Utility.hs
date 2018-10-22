module Utility where

import Text.Megaparsec


surroundedBy surround = between surround surround


-- Modified pattern from Text.Megaparsec (atEnd)
didConsume p = option False $ True <$ p
