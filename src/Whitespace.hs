module Whitespace where

import Data.Char (isSpace)
import Data.Functor (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import ParserDefinition


-- Modified from the space1 definition.
spacePreserveNewlines :: Parser ()
spacePreserveNewlines =
    void $ takeWhile1P (Just "white space") p
  where
    p c =
        isSpace c && c /= '\n' && c /= '\r'


-- Adhering to the convention suggested by Text.Megaparsec.Char.Lexer where
-- lexeme parsers assume no space leading a lexeme and consumes all trailing space.
--
-- General lexer method here is based on [1] and megaparsec docs.
-- [1] https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
spaceConsumer :: Parser () -> Parser ()
spaceConsumer s =
    let
        lineComment =
            L.skipLineComment "--"
        blockComment =
            L.skipBlockCommentNested "{-" "-}"
    in
        L.space s lineComment blockComment


lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ spaceConsumer spacePreserveNewlines


symbol = L.symbol $ spaceConsumer spacePreserveNewlines


symbolNewline = L.symbol $ spaceConsumer space1


nonIndented = L.nonIndented $ spaceConsumer spacePreserveNewlines
