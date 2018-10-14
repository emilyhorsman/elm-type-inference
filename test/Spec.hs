import Control.Monad (liftM2)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import Lib


main :: IO ()
main = hspec $ do
    describe "bool" $ do
        it "returns True" $ do
            parse bool "" "True" `shouldParse` True

        it "returns False" $ do
            parse bool "" "False" `shouldParse` False

        it "fails" $ do
            parse bool "" `shouldFailOn` "asdf"

    describe "singleChar" $ do
        it "returns escaped char" $ do
            parse singleChar "" "'\\n'" `shouldParse` '\n'
            parse singleChar "" "'\\r'" `shouldParse` '\r'
            parse singleChar "" "'\\t'" `shouldParse` '\t'
            parse singleChar "" "'\\\"'" `shouldParse` '"'
            parse singleChar "" "'\\''" `shouldParse` '\''
            parse singleChar "" "'\\\\'" `shouldParse` '\\'

        it "returns char" $ do
            parse singleChar "" "'a'" `shouldParse` 'a'

        it "fails on invalid escape sequence" $ do
            parse singleChar "" `shouldFailOn` "'\\a'"

        it "fails on unescaped quote" $ do
            parse singleChar "" `shouldFailOn` "'''"

    describe "singleLineString" $ do
        it "returns string" $ do
            parse singleLineString "" "\"Hello\"" `shouldParse` "Hello"

        it "returns string with escape sequences" $ do
            parse singleLineString "" "\"Hello\\n\"" `shouldParse` "Hello\n"
            parse singleLineString "" "\"Hello\\\"\"" `shouldParse` "Hello\""
            parse singleLineString "" "\" \\t \\r \\n \"" `shouldParse` " \t \r \n "

        it "cannot have a newline" $ do
            parse singleLineString "" `shouldFailOn` "Hello\nGoodbye"
            parse singleLineString "" `shouldFailOn` "Hello\rGoodbye"

    describe "multiLineString" $ do
        it "returns string" $ do
            parse multiLineString "" "\"\"\"Hello\nGoodbye\"\"\"" `shouldParse` "Hello\nGoodbye"

    describe "numberLiteral" $ do
        it "returns number" $ do
            parse numberLiteral "" "42" `shouldParse` 42
            parse (liftM2 (,) numberLiteral (char '.')) "" "42." `shouldParse` (42, '.')

        it "fails on sign" $ do
            parse numberLiteral "" `shouldFailOn` "-4"

        it "accepts list of numbers" $ do
            parse (sepBy numberLiteral (char ',')) "" "4,4" `shouldParse` [4,4]
