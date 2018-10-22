import Control.Monad (liftM2)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import AST
import Lib
import Literals
import Strings


main :: IO ()
main = hspec $ do
    describe "boolLiteral" $ do
        it "returns True" $
            parse boolLiteral "" "True" `shouldParse` Bool True

        it "returns False" $
            parse boolLiteral "" "False" `shouldParse` Bool False

        it "fails" $
            parse boolLiteral "" `shouldFailOn` "not a bool!"

    describe "charLiteral" $ do
        it "returns escaped char" $ do
            parse charLiteral "" "'\\n'" `shouldParse` Char '\n'
            parse charLiteral "" "'\\r'" `shouldParse` Char '\r'
            parse charLiteral "" "'\\t'" `shouldParse` Char '\t'
            parse charLiteral "" "'\\\"'" `shouldParse` Char '"'
            parse charLiteral "" "'\\''" `shouldParse` Char '\''
            parse charLiteral "" "'\\\\'" `shouldParse` Char '\\'

        it "returns char" $
            parse charLiteral "" "'a'" `shouldParse` Char 'a'

        it "fails on invalid escape sequence" $
            parse charLiteral "" `shouldFailOn` "'\\a'"

        it "fails on unescaped quote" $
            parse charLiteral "" `shouldFailOn` "'''"

    describe "singleLineStringLiteral" $ do
        it "returns string" $
            parse singleLineStringLiteral "" "\"Hello\"" `shouldParse` String "Hello"

        it "returns string with escape sequences" $ do
            parse singleLineStringLiteral "" "\"Hello\\n\"" `shouldParse` String "Hello\n"
            parse singleLineStringLiteral "" "\"Hello\\\"\"" `shouldParse` String "Hello\""
            parse singleLineStringLiteral "" "\" \\t \\r \\n \"" `shouldParse` String " \t \r \n "

        it "cannot have a newline" $ do
            parse singleLineStringLiteral "" `shouldFailOn` "Hello\nGoodbye"
            parse singleLineStringLiteral "" `shouldFailOn` "Hello\rGoodbye"

    describe "multiLineStringLiteral" $
        it "returns string" $
            parse multiLineStringLiteral "" "\"\"\"Hello\nGoodbye\"\"\"" `shouldParse` String "Hello\nGoodbye"

    describe "numberLexeme" $ do
        it "returns number" $ do
            parse numberLexeme "" "42" `shouldParse` 42
            parse (liftM2 (,) numberLexeme (char '.')) "" "42." `shouldParse` (42, '.')

        it "fails on sign" $
            parse numberLexeme "" `shouldFailOn` "-4"

        it "accepts list of numbers" $
            parse (sepBy numberLexeme (char ',')) "" "4,4" `shouldParse` [4,4]

    describe "floatLexeme" $
        it "returns float" $
            parse floatLexeme "" "4.5" `shouldParse` 4.5

    describe "numberLiteral" $ do
        it "returns float" $
            parse numberLiteral "" "4.5" `shouldParse` Float 4.5

        it "returns int" $
            parse numberLiteral "" "4" `shouldParse` Int 4

        it "fails on trailing ." $
            parse numberLiteral "" `shouldFailOn` "4."

    describe "identifier" $ do
        it "fails on reserved word" $
            parse identifier "" `shouldFailOn` "if"

        it "must start with lowercase letter" $
            parse identifier "" `shouldFailOn` "Variable"

        it "accepts identifier" $
            parse identifier "" "foobar " `shouldParse` "foobar"

    describe "listLiteral" $ do
        it "parses an empty list" $
            parse listLiteral "" "[]" `shouldParse` []

        it "parses a list of bools" $
            parse listLiteral "" "[ True, False, True ]" `shouldParse` [Bool True, Bool False, Bool True]

        it "parses a list of numbers" $ do
            parse listLiteral "" "[1, 2, 3]" `shouldParse` [Int 1, Int 2, Int 3]
            parse listLiteral "" "[1.0, 2.0, 3.0]" `shouldParse` [Float 1, Float 2, Float 3]

        it "fails when member fails" $
            parse listLiteral "" `shouldFailOn` "[4.]"

    describe "tupleLiteral" $ do
        it "parses an empty tuple" $
            parse tupleLiteral "" "()" `shouldParse` []

        it "parses a tuple of bools" $
            parse tupleLiteral "" "( True, False )" `shouldParse` [Bool True, Bool False]

    describe "ifExpression" $ do
        it "parses an if statement" $
            parse ifExpression "" "if True then 0 else 1" `shouldParse` If (Bool True) (Int 0) (Int 1)

        it "parses a nested predicate" $
            -- This actually works in the official Elm parser!
            parse ifExpression "" "if if True then True else False then 0 else 1" `shouldParse` If (If (Bool True) (Bool True) (Bool False)) (Int 0) (Int 1)

        it "parses a nested true result" $
            parse ifExpression "" "if True then if True then 0 else 1 else 2" `shouldParse` If (Bool True) (If (Bool True) (Int 0) (Int 1)) (Int 2)

        it "parses a nested false result" $
            parse ifExpression "" "if True then 0 else if True then 1 else 2" `shouldParse` If (Bool True) (Int 0) (If (Bool True) (Int 1) (Int 2))

        it "parses a multiline if expression" $
            parse ifExpression "" multiLineIfExpression `shouldParse`
                If (Bool True) (Int 0) (Int 1)

    describe "function" $ do
        it "parses a simple nullary function" $ do
            parse function "" "x = \"hello\"" `shouldParse` BoundFunctionDefinition "x" [] (String "hello")
            parse function "" "x = 'c'" `shouldParse` BoundFunctionDefinition "x" [] (Char 'c')
            parse function "" "x = True" `shouldParse` BoundFunctionDefinition "x" [] (Bool True)
            parse function "" "x = 1" `shouldParse` BoundFunctionDefinition "x" [] (Int 1)
            parse function "" "x = 1.5" `shouldParse` BoundFunctionDefinition "x" [] (Float 1.5)

        it "parses a simple ternary function" $
            parse function "" "x a b c = 1" `shouldParse` BoundFunctionDefinition "x" ["a", "b", "c"] (Int 1)

        it "fails on invalid parameter names" $
            parse function "" `shouldFailOn` "x 1 = 1"

    describe "functionApplication" $ do
        it "applies a nullary function" $
            parse functionApplication "" "x" `shouldParse` FunctionApplication "x" []

        it "uses juxtaposition for argument application" $
            parse functionApplication "" "x 1 2 3" `shouldParse` FunctionApplication "x" [Int 1, Int 2, Int 3]

    describe "anonymousFunction" $ do
        it "parses an anonymous function expression" $
            parse anonymousFunction "" "\\a -> 1" `shouldParse` AnonymousFunction ["a"] (Int 1)

        it "parses underscores for parameters" $
            parse anonymousFunction "" "\\_ a _ -> 1" `shouldParse` AnonymousFunction ["_", "a", "_"] (Int 1)

    describe "letBinding" $ do
        it "parses a single binding" $
            parse letBinding "" "let x = True in x" `shouldParse`
                LetBinding
                    [BoundFunctionDefinition "x" [] (Bool True)]
                    (FunctionApplication "x" [])

        it "parses multiple bindings separated by newlines" $
            let
                result =
                    LetBinding
                        [ BoundFunctionDefinition "x" [] (Bool True)
                        , BoundFunctionDefinition "y" [] (Bool True)
                        ]
                        (Int 0)
             in do
                parse letBinding "A" letBindingTwoBindingsA `shouldParse` result
                parse letBinding "B" letBindingTwoBindingsB `shouldParse` result
                parse letBinding "C" letBindingTwoBindingsC `shouldParse` result

        it "fails on mismatching indentation" $ do
            parse letBinding "A" `shouldFailOn` letBindingTwoBindingsInvalidIndentationA
            parse letBinding "B" `shouldFailOn` letBindingTwoBindingsInvalidIndentationB

    describe "caseExpression" $ do
        it "parses a single case" $
            parse caseExpression "" "case True of 1 -> 1" `shouldParse`
                Case (Bool True) [CaseBranch (Int 1) (Int 1)]

        it "parses multiple patterns separated by newlines" $
            let
                result =
                    Case
                        (FunctionApplication "foo" [])
                        [ CaseBranch (Int 1) (Int 1)
                        , CaseBranch (Int 2) (Int 2)
                        ]
             in do
                parse caseExpression "A" caseMultiplePatternsA `shouldParse` result
                parse caseExpression "B" caseMultiplePatternsB `shouldParse` result
                parse caseExpression "C" caseMultiplePatternsC `shouldParse` result

        it "fails on mismatching indentation" $
            parse caseExpression "" `shouldFailOn` caseInvalidIndentation

    describe "recordValue" $ do
        it "parses an empty record" $
            parse recordValue "" "{}" `shouldParse` RecordValue Map.empty

        it "parses a record" $
            parse recordValue "" "{ x = 1, y = 2 }" `shouldParse`
                RecordValue (Map.fromList [("x", Int 1), ("y", Int 2)])

    describe "recordUpdate" $ do
        it "updates a record" $
            parse recordUpdate "" "{ rec | x = 1 }" `shouldParse`
                RecordUpdate "rec" (Map.fromList [("x", Int 1)])

    describe "expression" $ do
        it "parses a nested tuple" $
            parse expression "" "(((True), False))" `shouldParse` Tuple [Tuple [Tuple [Bool True], Bool False]]

        it "parses a nested list" $
            parse expression "" "[[[True], [False]]]" `shouldParse` List [List [List [Bool True], List [Bool False]]]

        it "parses function application in the predicate of an if statement" $
            parse expression "" "if x 1 then 1 else 0" `shouldParse` If (FunctionApplication "x" [Int 1]) (Int 1) (Int 0)

        it "parses case and let in if" $
            -- TODO: This is semantically invalid but pattern matching isn't
            -- implemented yet.
            parse expression "" "if case x of 1 -> True then let a = 5 in a else 5" `shouldParse`
                If
                    (Case
                        (FunctionApplication "x" [])
                        [CaseBranch (Int 1) (Bool True)]
                    )
                    (LetBinding
                        [BoundFunctionDefinition "a" [] (Int 5)]
                        (FunctionApplication "a" [])
                    )
                    (Int 5)
