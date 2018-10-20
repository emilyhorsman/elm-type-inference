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
            parse (Bool <$> bool) "" "True" `shouldParse` (Bool True)

        it "returns False" $ do
            parse bool "" "False" `shouldParse` False

        it "fails" $ do
            parse bool "" `shouldFailOn` "not a bool!"

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

    describe "floatLiteral" $ do
        it "returns float" $ do
            parse floatLiteral "" "4.5" `shouldParse` 4.5

    describe "identifier" $ do
        it "fails on reserved word" $ do
            parse identifier "" `shouldFailOn` "if"

        it "must start with lowercase letter" $ do
            parse identifier "" `shouldFailOn` "Variable"

        it "accepts identifier" $ do
            parse identifier "" "foobar " `shouldParse` "foobar"

    describe "numberWrapper" $ do
        it "returns float" $ do
            parse numberWrapper "" "4.5" `shouldParse` Float 4.5

        it "returns int" $ do
            parse numberWrapper "" "4" `shouldParse` Int 4

        it "fails on trailing ." $ do
            parse numberWrapper "" `shouldFailOn` "4."

    describe "function" $ do
        it "parses a simple nullary function" $ do
            parse function "" "x = \"hello\"" `shouldParse` (BoundFunctionDefinition "x" [] (String "hello"))
            parse function "" "x = 'c'" `shouldParse` (BoundFunctionDefinition "x" [] (Char 'c'))
            parse function "" "x = True" `shouldParse` (BoundFunctionDefinition "x" [] (Bool True))
            parse function "" "x = 1" `shouldParse` (BoundFunctionDefinition "x" [] (Int 1))
            parse function "" "x = 1.5" `shouldParse` (BoundFunctionDefinition "x" [] (Float 1.5))

        it "parses a simple ternary function" $ do
            parse function "" "x a b c = 1" `shouldParse` (BoundFunctionDefinition "x" ["a", "b", "c"] (Int 1))
