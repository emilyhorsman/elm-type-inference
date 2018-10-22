import Control.Monad (liftM2)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import Lib
import Strings


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

    {-describe "listLiteral" $ do
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
            -}

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

    describe "reference" $
        it "parses an identifier" $
            parse reference "" "x" `shouldParse` Reference "x"

    describe "functionApplication" $ do
        it "parses a unary function" $
            parse functionApplication "" "f 1" `shouldParse`
                FunctionApplication (Reference "f") [Int 1]

        it "is left associative" $
            parse functionApplication "" "f g 1" `shouldParse`
                FunctionApplication
                    (Reference "f")
                    [ Reference "g"
                    , Int 1
                    ]

        it "expressions can be applied" $
            parse functionApplication "" "(if True then f else g) 1" `shouldParse`
                FunctionApplication
                    (If (Bool True) (Reference "f") (Reference "g"))
                    [Int 1]

        it "parenthesis for precedence in argument" $
            parse functionApplication "" "f (g 1)" `shouldParse`
                FunctionApplication
                    (Reference "f")
                    [ FunctionApplication
                        (Reference "g")
                        [Int 1]
                    ]

        it "deeper parenthesis for precedence in argument" $
            parse functionApplication "" "f ((g a) (h (id 1)))" `shouldParse`
                FunctionApplication
                    (Reference "f")
                    [ FunctionApplication
                        (FunctionApplication (Reference "g") [Reference "a"])
                        [ FunctionApplication
                            (Reference "h")
                            [ FunctionApplication (Reference "id") [Int 1] ]
                        ]
                    ]

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
                    (Reference "x")

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
                Cases (Bool True) [Case (Int 1) (Int 1)]

        it "parses multiple patterns separated by newlines" $
            let
                result =
                    Cases
                        (Reference "foo")
                        [ Case (Int 1) (Int 1)
                        , Case (Int 2) (Int 2)
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
        {-it "parses a nested tuple" $
            parse expression "" "(((True), False))" `shouldParse` Tuple [Tuple [Tuple [Bool True], Bool False]]

        it "parses a nested list" $
            parse expression "" "[[[True], [False]]]" `shouldParse` List [List [List [Bool True], List [Bool False]]]-}

        it "parses function application in the predicate of an if statement" $
            parse expression "" "if x 1 then 1 else 0" `shouldParse`
                If
                    (FunctionApplication (Reference "x") [Int 1])
                    (Int 1)
                    (Int 0)

        it "parses case and let in if" $
            -- TODO: This is semantically invalid but pattern matching isn't
            -- implemented yet.
            parse expression "" "if case x of 1 -> True then let a = 5 in a else 5" `shouldParse`
                If
                    (Cases
                        (Reference "x")
                        [Case (Int 1) (Bool True)]
                    )
                    (LetBinding
                        [BoundFunctionDefinition "a" [] (Int 5)]
                        (Reference "a")
                    )
                    (Int 5)
