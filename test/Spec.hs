import Control.Monad (liftM2)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import Lib


main :: IO ()
main = hspec $ do
    describe "bool" $ do
        it "returns True" $
            parse bool "" "True" `shouldParse` True

        it "returns False" $
            parse bool "" "False" `shouldParse` False

        it "fails" $
            parse bool "" `shouldFailOn` "not a bool!"

    describe "singleChar" $ do
        it "returns escaped char" $ do
            parse singleChar "" "'\\n'" `shouldParse` '\n'
            parse singleChar "" "'\\r'" `shouldParse` '\r'
            parse singleChar "" "'\\t'" `shouldParse` '\t'
            parse singleChar "" "'\\\"'" `shouldParse` '"'
            parse singleChar "" "'\\''" `shouldParse` '\''
            parse singleChar "" "'\\\\'" `shouldParse` '\\'

        it "returns char" $
            parse singleChar "" "'a'" `shouldParse` 'a'

        it "fails on invalid escape sequence" $
            parse singleChar "" `shouldFailOn` "'\\a'"

        it "fails on unescaped quote" $
            parse singleChar "" `shouldFailOn` "'''"

    describe "singleLineString" $ do
        it "returns string" $
            parse singleLineString "" "\"Hello\"" `shouldParse` "Hello"

        it "returns string with escape sequences" $ do
            parse singleLineString "" "\"Hello\\n\"" `shouldParse` "Hello\n"
            parse singleLineString "" "\"Hello\\\"\"" `shouldParse` "Hello\""
            parse singleLineString "" "\" \\t \\r \\n \"" `shouldParse` " \t \r \n "

        it "cannot have a newline" $ do
            parse singleLineString "" `shouldFailOn` "Hello\nGoodbye"
            parse singleLineString "" `shouldFailOn` "Hello\rGoodbye"

    describe "multiLineString" $
        it "returns string" $
            parse multiLineString "" "\"\"\"Hello\nGoodbye\"\"\"" `shouldParse` "Hello\nGoodbye"

    describe "numberLiteral" $ do
        it "returns number" $ do
            parse numberLiteral "" "42" `shouldParse` 42
            parse (liftM2 (,) numberLiteral (char '.')) "" "42." `shouldParse` (42, '.')

        it "fails on sign" $
            parse numberLiteral "" `shouldFailOn` "-4"

        it "accepts list of numbers" $
            parse (sepBy numberLiteral (char ',')) "" "4,4" `shouldParse` [4,4]

    describe "floatLiteral" $
        it "returns float" $
            parse floatLiteral "" "4.5" `shouldParse` 4.5

    describe "identifier" $ do
        it "fails on reserved word" $
            parse identifier "" `shouldFailOn` "if"

        it "must start with lowercase letter" $
            parse identifier "" `shouldFailOn` "Variable"

        it "accepts identifier" $
            parse identifier "" "foobar " `shouldParse` "foobar"

    describe "numberWrapper" $ do
        it "returns float" $
            parse numberWrapper "" "4.5" `shouldParse` Float 4.5

        it "returns int" $
            parse numberWrapper "" "4" `shouldParse` Int 4

        it "fails on trailing ." $
            parse numberWrapper "" `shouldFailOn` "4."

    describe "function" $ do
        it "parses a simple nullary function" $ do
            parse function "" "x = \"hello\"" `shouldParse` BoundFunctionDefinition "x" [] (String "hello")
            parse function "" "x = 'c'" `shouldParse` BoundFunctionDefinition "x" [] (Char 'c')
            parse function "" "x = True" `shouldParse` BoundFunctionDefinition "x" [] (Bool True)
            parse function "" "x = 1" `shouldParse` BoundFunctionDefinition "x" [] (Int 1)
            parse function "" "x = 1.5" `shouldParse` BoundFunctionDefinition "x" [] (Float 1.5)

        it "parses a simple ternary function" $
            parse function "" "x a b c = 1" `shouldParse` BoundFunctionDefinition "x" ["a", "b", "c"] (Int 1)
