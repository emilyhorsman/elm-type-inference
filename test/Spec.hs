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

    describe "listExpression" $ do
        it "parses an empty list" $
            parse listExpression "" "[]" `shouldParse` List []

        it "parses a list of bools" $
            parse listExpression "" "[ True, False, True ]" `shouldParse`
                List [Bool True, Bool False, Bool True]

        it "parses a list of numbers" $ do
            parse listExpression "" "[1, 2, 3]" `shouldParse`
                List [Int 1, Int 2, Int 3]
            parse listExpression "" "[1.0, 2.0, 3.0]" `shouldParse`
                List [Float 1, Float 2, Float 3]

        it "fails when member fails" $
            parse listExpression "" `shouldFailOn` "[4.]"

    describe "tupleExpression" $ do
        it "parses a pair of bools" $
            parse tupleExpression "" "( True, False )" `shouldParse`
                Tuple [Bool True, Bool False]

        it "parses a triple of bools" $
            parse tupleExpression "" "( True, False, False )" `shouldParse`
                Tuple [Bool True, Bool False, Bool False]

        it "fails on empty tuple" $
            parse tupleExpression "" `shouldFailOn` "()"

        it "fails on single member tuple" $
            parse tupleExpression "" `shouldFailOn` "(1)"

        it "fails on k > 3 tuple" $
            parse tupleExpression "" `shouldFailOn` "(1,2,3,4)"

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
                    (Variable "x")

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
                        (Variable "foo")
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

    describe "variable" $ do
        it "parses an identifier" $
            parse variable "" "foo" `shouldParse` (Variable "foo")

        it "parses a record accessor" $
            parse variable "" ".x" `shouldParse` (RecordAccess "x")

        it "parses a qualified record accessor" $
            parse variable "" "foo.x" `shouldParse`
                QualifiedRecordAccess "foo" "x"

        it "parses a qualified module accessor" $
            parse variable "" "List.map" `shouldParse`
                QualifiedModuleAccess "List" "map"

    describe "expression" $ do
        it "degrades into term" $ do
            parse expression "" "1" `shouldParse` (Int 1)
            parse expression "" "x" `shouldParse` (Variable "x")

        it "parses a unary function" $
            parse expression "" "x 1" `shouldParse`
                FunctionApplication (Variable "x") (Int 1)

        it "parses function application in the predicate of an if statement" $
            parse expression "" "if x 1 then 1 else 0" `shouldParse`
                If (FunctionApplication (Variable "x") (Int 1)) (Int 1) (Int 0)

        it "has left associative function application" $
            parse expression "" "f g 1 2" `shouldParse`
                FunctionApplication
                    (FunctionApplication
                        (FunctionApplication (Variable "f") (Variable "g"))
                        (Int 1)
                    )
                    (Int 2)

        it "parenthesis override precedence" $
            parse expression "" "f (g 1)" `shouldParse`
                FunctionApplication
                    (Variable "f")
                    (FunctionApplication (Variable "g") (Int 1))

        it "parses expression as lvalue of function application" $
            parse expression "" "(if True then f else g) 1" `shouldParse`
                FunctionApplication
                    (If (Bool True) (Variable "f") (Variable "g"))
                    (Int 1)

        it "parses anonymous function as lvalue of function application" $
            parse expression "" "(\\x -> x) 1" `shouldParse`
                FunctionApplication
                    (AnonymousFunction ["x"] (Variable "x"))
                    (Int 1)

        it "parses case and let in if" $
            -- TODO: This is semantically invalid but pattern matching isn't
            -- implemented yet.
            parse expression "" "if case x of 1 -> True then let a = 5 in a else 5" `shouldParse`
                If
                    (Case
                        (Variable "x")
                        [CaseBranch (Int 1) (Bool True)]
                    )
                    (LetBinding
                        [BoundFunctionDefinition "a" [] (Int 5)]
                        (Variable "a")
                    )
                    (Int 5)

        it "parses a nested tuple" $
            parse expression "" "((1, 2), (1, 2))" `shouldParse`
                Tuple
                    [ Tuple [ Int 1, Int 2 ]
                    , Tuple [ Int 1, Int 2 ]
                    ]

        it "parses a nested list" $
            parse expression "" "[[[True], [False]]]" `shouldParse`
                List [List [List [Bool True], List [Bool False]]]

        it "parses addition" $
            parse expression "" "a + b" `shouldParse`
                BinOp Add (Variable "a") (Variable "b")

        it "parses function application with higher precedence than addition" $
            parse expression "" "f a + f b" `shouldParse`
                BinOp
                    Add
                    (FunctionApplication (Variable "f") (Variable "a"))
                    (FunctionApplication (Variable "f") (Variable "b"))

        it "parses minus" $
            parse expression "" "a - b" `shouldParse`
                BinOp Minus (Variable "a") (Variable "b")

        it "multiply binds tighter than addition" $
            parse expression "" "a + b * c" `shouldParse`
                BinOp
                    Add
                    (Variable "a")
                    (BinOp Multiply (Variable "b") (Variable "c"))

        it "parses exponentiation" $
            parse expression "" "2^3^f 1" `shouldParse`
                BinOp
                    Exponentiation
                    (Int 2)
                    (BinOp
                        Exponentiation
                        (Int 3)
                        (FunctionApplication (Variable "f") (Int 1))
                    )

        it "has non-associative comparison operators" $
            parse expression "" `shouldFailOn` "a == b /= c"

        it "has boolean operators" $
            parse expression "" "if a && b || c then 1 else 0" `shouldParse`
                If
                    (BinOp
                        BooleanOr
                        (BinOp BooleanAnd (Variable "a") (Variable "b"))
                        (Variable "c")
                    )
                    (Int 1)
                    (Int 0)

        it "has cons and append" $
            parse expression "" "x :: y :: [] ++ z" `shouldParse`
                BinOp
                    Cons
                    (Variable "x")
                    (BinOp
                        Cons
                        (Variable "y")
                        (BinOp Append (List []) (Variable "z"))
                    )

        it "has rightwards composition and application" $
            parse expression "" "1 |> f >> f" `shouldParse`
                BinOp
                    ApplyRight
                    (Int 1)
                    (BinOp
                        ComposeRight
                        (Variable "f")
                        (Variable "f")
                    )

        it "has leftwards composition and application" $
            parse expression "" "f << f <| 1" `shouldParse`
                BinOp
                    ApplyLeft
                    (BinOp
                        ComposeLeft
                        (Variable "f")
                        (Variable "f")
                    )
                    (Int 1)

        it "handles unary minus" $
            parse expression "" "-1" `shouldParse`
                Negate (Int 1)

        it "handles unary minus expression" $
            parse expression "" "-(a + b)" `shouldParse`
                Negate (BinOp Add (Variable "a") (Variable "b"))

        it "fails on space after unary minus" $
            parse expression "" `shouldFailOn` "- 1"

        it "handles > and <" $
            parse expression "" "1 < 2 && 2 > 1" `shouldParse`
                BinOp
                    BooleanAnd
                    (BinOp LessThan (Int 1) (Int 2))
                    (BinOp GreaterThan (Int 2) (Int 1))
