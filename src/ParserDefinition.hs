module ParserDefinition where


import Data.Void
import Text.Megaparsec


-- The Text.Megaparsec says it's conventional to have a type synonym such as
-- this one.
type Parser = Parsec Void String
