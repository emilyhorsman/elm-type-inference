module ParseSpec (spec) where

import Control.Monad (liftM2)
import qualified Data.Map.Strict as Map
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

import AST
import Lib
import Literals
import Operators (operators)
import Strings
import TestUtility


spec :: Spec
spec = do
    describe "boolLiteral" $ do
        it "returns True" $
            parse boolLiteral "" "True" `shouldParse` True

        it "returns False" $
            parse boolLiteral "" "False" `shouldParse` False

        it "fails" $
            parse boolLiteral "" `shouldFailOn` "not a bool!"

    describe "charLiteral" $ do
        it "returns escaped char" $ do
            parse charLiteral "" "'\\n'" `shouldParse` '\n'
            parse charLiteral "" "'\\r'" `shouldParse` '\r'
            parse charLiteral "" "'\\t'" `shouldParse` '\t'
            parse charLiteral "" "'\\\"'" `shouldParse` '"'
            parse charLiteral "" "'\\''" `shouldParse` '\''
            parse charLiteral "" "'\\\\'" `shouldParse` '\\'

        it "returns char" $
            parse charLiteral "" "'a'" `shouldParse` 'a'

        it "fails on invalid escape sequence" $
            parse charLiteral "" `shouldFailOn` "'\\a'"

        it "fails on unescaped quote" $
            parse charLiteral "" `shouldFailOn` "'''"

    describe "singleLineStringLiteral" $ do
        it "returns string" $
            parse singleLineStringLiteral "" "\"Hello\"" `shouldParse` "Hello"

        it "returns string with escape sequences" $ do
            parse singleLineStringLiteral "" "\"Hello\\n\"" `shouldParse` "Hello\n"
            parse singleLineStringLiteral "" "\"Hello\\\"\"" `shouldParse` "Hello\""
            parse singleLineStringLiteral "" "\" \\t \\r \\n \"" `shouldParse` " \t \r \n "

        it "cannot have a newline" $ do
            parse singleLineStringLiteral "" `shouldFailOn` "Hello\nGoodbye"
            parse singleLineStringLiteral "" `shouldFailOn` "Hello\rGoodbye"

    describe "multiLineStringLiteral" $
        it "returns string" $
            parse multiLineStringLiteral "" "\"\"\"Hello\nGoodbye\"\"\"" `shouldParse` "Hello\nGoodbye"

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

        it "parses a multiline list" $
            let
                result =
                    List [Int 1, Int 2, Int 3]
            in do
                parse listExpression "A" listExpressionMultilineA `shouldParse` result
                parse listExpression "B" listExpressionMultilineB `shouldParse` result

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

        it "parses a multiline tuple" $
            let
                result =
                    Tuple [Int 1, Int 2, Int 3]
            in do
                parse tupleExpression "A" tupleExpressionMultilineA `shouldParse` result
                parse tupleExpression "B" tupleExpressionMultilineB `shouldParse` result

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
            parse function "" "x = \"hello\"" `shouldParse`
                BoundFunctionDefinition Nothing "x" [] (String "hello")

            parse function "" "x = 'c'" `shouldParse`
                BoundFunctionDefinition Nothing "x" [] (Char 'c')

            parse function "" "x = True" `shouldParse`
                BoundFunctionDefinition Nothing "x" [] (Bool True)

            parse function "" "x = 1" `shouldParse`
                BoundFunctionDefinition Nothing "x" [] (Int 1)

            parse function "" "x = 1.5" `shouldParse`
                BoundFunctionDefinition Nothing "x" [] (Float 1.5)

        it "parses a simple ternary function" $
            parse function "" "x a b c = 1" `shouldParse`
                BoundFunctionDefinition
                    Nothing
                    "x"
                    [PatternVariable "a", PatternVariable "b", PatternVariable "c"]
                    (Int 1)

        it "parses a pattern literal" $
            parse function "" "x 1 = 1" `shouldParse`
                BoundFunctionDefinition Nothing "x" [PatternInt 1] (Int 1)

        it "fails on cons not surrounded in parenthesis" $
            parse function "" `shouldFailOn` "func x :: xs = 1"

        it "parses a cons pattern in parenthesis" $
            parse function "" "func (x :: y :: []) = 1" `shouldParse`
                BoundFunctionDefinition
                    Nothing
                    "func"
                    [ PatternCons (PatternVariable "x")
                        (PatternCons (PatternVariable "y") (PatternList []))
                    ]
                    (Int 1)

        it "parses destructured parameters" $
            parse function "" "func (x, y) {a, b} = 1" `shouldParse`
                BoundFunctionDefinition
                    Nothing
                    "func"
                    [ PatternTuple [PatternVariable "x", PatternVariable "y"]
                    , PatternRecord ["a", "b"]
                    ]
                    (Int 1)

        it "parses char and string matching" $
            parse function "" "func 'x' \"hello\" _ = 1" `shouldParse`
                BoundFunctionDefinition
                    Nothing
                    "func"
                    [ PatternChar 'x'
                    , PatternString "hello"
                    , PatternAnything
                    ]
                    (Int 1)

        it "parses a unary type annotation" $
            parse function "" funcTypeUnaryAnnotation `shouldParse`
                BoundFunctionDefinition
                    (Just
                        (Type "Int" [])
                    )
                    "x"
                    []
                    (Int 1)

        it "parses a type annotation" $
            parse function "" funcTypeAnnotation `shouldParse`
                BoundFunctionDefinition
                    (Just
                        (TypeFunc
                            (Type "Int" [])
                            (TypeFunc
                                (Type "Int" [])
                                (TupleType [Type "Int" [], Type "Int" []])
                            )
                        )
                    )
                    "x"
                    [ PatternAnything
                    , PatternAnything
                    ]
                    (Tuple [Int 1, Int 2])

        it "parses a type annotation with precedence" $
            parse function "" funcTypeAnnotationNested `shouldParse`
                BoundFunctionDefinition
                    (Just
                        (TypeFunc
                            (TypeFunc
                                (Type "Int" [])
                                (Type "Int" [])
                            )
                            (Type "Int" [])
                        )
                    )
                    "x"
                    [ PatternAnything ]
                    (Int 1)

        it "parses a type annotation with arguments" $
            parse function "" funcTypeAnnotationArgs `shouldParse`
                BoundFunctionDefinition
                    (Just
                        (Type "Maybe" [TypeArg "a"])
                    )
                    "x"
                    []
                    (Constructor "Nothing")

        it "parses a type annotation with constrained type variables" $
            (snd <$> parse declarationAnnotation "" "foo : number -> appendable") `shouldParse`
                TypeFunc
                    (ConstrainedTypeVariable Number)
                    (ConstrainedTypeVariable Appendable)

    describe "anonymousFunction" $ do
        it "parses an anonymous function expression" $
            parse anonymousFunction "" "\\a -> 1" `shouldParse`
                AnonymousFunction [PatternVariable "a"] (Int 1)

        it "parses underscores for parameters" $
            parse anonymousFunction "" "\\_ a _ -> 1" `shouldParse`
                AnonymousFunction
                    [ PatternAnything
                    , PatternVariable "a"
                    , PatternAnything
                    ]
                    (Int 1)

    describe "letBinding" $ do
        it "parses a single binding" $
            parse letBinding "" "let x = True in x" `shouldParse`
                LetBinding
                    [BoundFunctionDefinition Nothing "x" [] (Bool True)]
                    (Variable "x")

        it "parses multiple bindings separated by newlines" $
            let
                result =
                    LetBinding
                        [ BoundFunctionDefinition Nothing "x" [] (Bool True)
                        , BoundFunctionDefinition Nothing "y" [] (Bool True)
                        ]
                        (Int 0)
             in do
                parse letBinding "A" letBindingTwoBindingsA `shouldParse` result
                parse letBinding "B" letBindingTwoBindingsB `shouldParse` result
                parse letBinding "C" letBindingTwoBindingsC `shouldParse` result

        it "fails on mismatching indentation" $ do
            parse letBinding "A" `shouldFailOn` letBindingTwoBindingsInvalidIndentationA
            parse letBinding "B" `shouldFailOn` letBindingTwoBindingsInvalidIndentationB

        it "parses a type annotation" $ do
            parse letBinding "" letBindingTypeAnnotation `shouldParse`
                LetBinding
                    [ BoundFunctionDefinition
                        (Just
                            (TypeFunc
                                (Type "Bool" [])
                                (Type "Int" [])
                            )
                        )
                        "x"
                        [PatternVariable "y"]
                        (Int 1)
                    ]
                    (FunctionApplication (Variable "x") (Bool True))

        it "correctly handles multiline function application" $
            parse letBinding "" letBindingMultilineFunApp `shouldParse`
                LetBinding
                    [ BoundFunctionDefinition
                        Nothing
                        "x"
                        []
                        (Int 1)
                    ]
                    (LetBinding
                        [ BoundFunctionDefinition
                            Nothing
                            "y"
                            []
                            (FunctionApplication
                                (FunctionApplication
                                    (Variable "f")
                                    (Int 1)
                                )
                                (Int 2)
                            )
                        , BoundFunctionDefinition
                            Nothing
                            "z"
                            []
                            (Int 3)
                        ]
                        (Variable "z")
                    )

    describe "caseExpression" $ do
        it "parses a single case" $
            parse caseExpression "" "case True of _ -> 1" `shouldParse`
                Case (Bool True) [CaseBranch PatternAnything (Int 1)]

        it "parses multiple patterns separated by newlines" $
            let
                result =
                    Case
                        (Variable "foo")
                        [ CaseBranch (PatternInt 1) (Int 1)
                        , CaseBranch PatternAnything (Int 2)
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

        it "parses a multiline record" $
            parse recordValue "" recordValueMultiline `shouldParse`
                RecordValue (Map.fromList [("x", Int 1), ("y", Int 2)])

    describe "recordUpdate" $ do
        it "updates a record" $
            parse recordUpdate "" "{ rec | x = 1 }" `shouldParse`
                RecordUpdate "rec" (Map.fromList [("x", Int 1)])

        it "parses a multiline record update" $
            parse recordUpdate "" recordUpdateMultiline `shouldParse`
                RecordUpdate "rec" (Map.fromList [("x", Int 1)])

    describe "variable" $ do
        it "parses an identifier" $
            parse variable "" "foo" `shouldParse` Variable "foo"

        it "parses a record accessor" $
            parse variable "" ".x" `shouldParse` RecordAccess "x"

        it "parses a qualified record accessor" $
            parse variable "" "foo.x" `shouldParse`
                QualifiedRecordAccess "foo" "x"

        it "parses a qualified module accessor" $
            parse variable "" "List.map" `shouldParse`
                QualifiedModuleAccess "List" "map"

        it "parses a reference to an operator in parenthesis" $
            let
                test op =
                    parse variable "" ("(" ++ op ++ ")") `shouldParse` Variable op
            in
                mapM_ test operators

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
                    (AnonymousFunction [PatternVariable "x"] (Variable "x"))
                    (Int 1)

        it "parses multiline function application" $
            let
                result =
                    FunctionApplication
                        (FunctionApplication
                            (Variable "div")
                            (List [])
                        )
                        (List [])
            in do
                parse expression "A" multiLineFunctionApplicationA `shouldParse` result
                parse expression "B" multiLineFunctionApplicationB `shouldParse` result

        it "parses case and let in if" $
            parse expression "" "if case x of _ -> True then let a = 5 in a else 5" `shouldParse`
                If
                    (Case
                        (Variable "x")
                        [CaseBranch (PatternAnything) (Bool True)]
                    )
                    (LetBinding
                        [BoundFunctionDefinition Nothing "a" [] (Int 5)]
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

        it "parses a record value" $
            parse expression "" "{ a = 1 }" `shouldParse`
                RecordValue (Map.fromList [("a", Int 1)])

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

        it "parses a non-trivial case expression" $
            parse expression "" caseNonTrivialPatterns `shouldParse`
                Case
                    (Variable "foo")
                    [ CaseBranch
                        (PatternCons
                            (PatternVariable "x")
                            (PatternCons (PatternVariable "y") (PatternList []))
                        )
                        (Tuple [Variable "x", Variable "y"])
                    , CaseBranch
                        (PatternAlias
                            (PatternCons (PatternVariable "x") (PatternVariable "xs"))
                            "list"
                        )
                        (Tuple [Variable "x", Variable "list"])
                    , CaseBranch
                        PatternAnything
                        (Tuple [Variable "foo", Variable "foo"])
                    ]

        it "parses data constructors" $
            parse expression "" "Just (Just 1)" `shouldParse`
                FunctionApplication (Constructor "Just")
                    (FunctionApplication (Constructor "Just") (Int 1))

    describe "pattern" $ do
        it "parses underscore for anything" $
            parse pattern "" "_" `shouldParse` PatternAnything

        it "parses variable" $
            parse pattern "" "foo" `shouldParse` PatternVariable "foo"

        it "fails on expression-like variables" $
            parse pattern "" `shouldFailOn` ".foo"

        it "parses an alias" $
            parse pattern "" "(a as b)" `shouldParse`
                PatternAlias (PatternVariable "a") "b"

        it "parses record destructuring" $
            parse pattern "" "{ a, b, c }" `shouldParse`
                PatternRecord ["a", "b", "c"]

        it "parses pair destructuring" $
            parse pattern "" "(a, _)" `shouldParse`
                PatternTuple [PatternVariable "a", PatternAnything]

        it "parses triple destructuring" $
            parse pattern "" "(a, _, b)" `shouldParse`
                PatternTuple [PatternVariable "a", PatternAnything, PatternVariable "b"]

        it "fails on empty tuple" $
            parse pattern "" `shouldFailOn` "()"

        it "fails on k=4 tuple" $
            parse pattern "" `shouldFailOn` "(a, b, c, d)"

        it "parses empty list" $
            parse pattern "" "[]" `shouldParse` PatternList []

        it "parses list destructuring" $
            parse pattern "" "[a, _, b]" `shouldParse`
                PatternList [PatternVariable "a", PatternAnything, PatternVariable "b"]

        it "parses cons destructuring" $
            parse pattern "" "x :: xs" `shouldParse`
                PatternCons (PatternVariable "x") (PatternVariable "xs")

        it "parses cons destructuring with right-associativity" $
            parse pattern "" "x :: y :: []" `shouldParse`
                PatternCons (PatternVariable "x")
                    (PatternCons (PatternVariable "y") (PatternList []))

        it "parses an aliased cons destructuring" $
            parse pattern "" "(x :: y as list)" `shouldParse`
                PatternAlias
                    (PatternCons (PatternVariable "x") (PatternVariable "y"))
                    "list"

        it "parses an aliased cons destructuring RHS" $
            -- Working on this is teaching me a lot about what Elm will allow.
            parse pattern "" "x :: (xs as b)" `shouldParse`
                PatternCons
                    (PatternVariable "x")
                    (PatternAlias (PatternVariable "xs") "b")

        it "parses superfluous parenthesis" $
            parse pattern "" "(x :: (xs as b))" `shouldParse`
                PatternCons
                    (PatternVariable "x")
                    (PatternAlias (PatternVariable "xs") "b")

        it "parses a data constructor" $
            parse pattern "" "Nothing" `shouldParse`
                PatternConstructor "Nothing" []

        it "parses a data constructor with arguments" $
            parse pattern "" "(Just x y)" `shouldParse`
                PatternConstructor "Just"
                    [PatternVariable "x", PatternVariable "y"]

        it "parses a nested data constructor" $
            parse pattern "" "(Just (Just {a, b}))" `shouldParse`
                PatternConstructor "Just"
                    [PatternConstructor "Just"
                        [PatternRecord ["a", "b"]
                        ]
                    ]

    describe "typeConstructorDefinition" $ do
        it "parses a single variant" $
            parse typeConstructorDefinition "" "type Foo = Bar" `shouldParse`
                TypeConstructorDefinition "Foo" [] [Variant "Bar" []]

        it "parses a variant with typed data" $
            parse typeConstructorDefinition "" "type Foo = Bar String Int" `shouldParse`
                TypeConstructorDefinition "Foo"
                    []
                    [Variant "Bar" [Type "String" [], Type "Int" []]]

        it "parses a type constructor argument" $
            parse typeConstructorDefinition "" "type Foo a = Bar a" `shouldParse`
                TypeConstructorDefinition
                    "Foo"
                    [TypeConstructorArg "a"]
                    [Variant "Bar" [TypeArg "a"]]

        it "parses a variant with nested types" $
            parse typeConstructorDefinition "" "type Foo a b = Bar Int (Maybe a) (Either a b)" `shouldParse`
                TypeConstructorDefinition
                    "Foo"
                    [TypeConstructorArg "a", TypeConstructorArg "b"]
                    [Variant
                        "Bar"
                        [ Type "Int" []
                        , Type "Maybe" [TypeArg "a"]
                        , Type "Either" [TypeArg "a", TypeArg "b"]
                        ]
                    ]

        it "parses a variant with deeply nested types" $
            parse typeConstructorDefinition "" "type Foo a = Bar (Maybe (Maybe a))" `shouldParse`
                TypeConstructorDefinition
                    "Foo"
                    [TypeConstructorArg "a"]
                    [Variant
                        "Bar"
                        [Type "Maybe"
                            [Type "Maybe" [TypeArg "a"]]
                        ]
                    ]

        it "parses multiple variants" $
            parse typeConstructorDefinition "" "type Maybe a = Nothing | Just a" `shouldParse`
                TypeConstructorDefinition
                    "Maybe"
                    [TypeConstructorArg "a"]
                    [ Variant "Nothing" []
                    , Variant "Just" [TypeArg "a"]
                    ]

        it "parses a tuple type" $
            parse typeConstructorDefinition "" "type Point a = Point (a, a)" `shouldParse`
                TypeConstructorDefinition
                    "Point"
                    [TypeConstructorArg "a"]
                    [Variant "Point" [TupleType [TypeArg "a", TypeArg "a"]]]

        it "parses a record type" $
            parse typeConstructorDefinition "" "type Point a = Point { x : a, y : a }" `shouldParse`
                TypeConstructorDefinition
                    "Point"
                    [TypeConstructorArg "a"]
                    [Variant
                        "Point"
                        [RecordType (Map.fromList [("x", TypeArg "a"), ("y", TypeArg "a")])]
                    ]

        it "fails with an invalid variant" $
            parse typeConstructorDefinition "" `shouldFailOn` "type Foo = (Int, Int)"

        it "fails without any variants" $ do
            parse typeConstructorDefinition "" `shouldFailOn` "type Foo"
            parse typeConstructorDefinition "" `shouldFailOn` "type Foo ="

    describe "typeAlias" $ do
        it "parses a simple type alias" $
            parse typeAlias "" "type alias Name = String" `shouldParse`
                TypeAlias "Name" (Type "String" [])

        it "parses a record type alias" $
            parse typeAlias "" "type alias Point = { x : Float, y : Float }" `shouldParse`
                TypeAlias "Point"
                    (RecordType
                        (Map.fromList [("x", Type "Float" []), ("y", Type "Float" [])]
                        )
                    )

    describe "moduleStatement" $ do
        it "parses a simple module statement" $
            parse moduleStatement "" "module Main exposing (..)" `shouldParse`
                ModuleStatement "Main" [AllSymbols]

        it "parses a module statement exposing particular symbols" $
            parse moduleStatement "" "module Main exposing (main, foo)" `shouldParse`
                ModuleStatement "Main"
                    [ ModuleFunction "main"
                    , ModuleFunction "foo"
                    ]

        it "parses a module statement exposing a qualified type" $
            parse moduleStatement "" "module Main exposing (main, Maybe)" `shouldParse`
                ModuleStatement "Main"
                    [ ModuleFunction "main"
                    , ModuleType "Maybe" []
                    ]

        it "parses a module statement exposing types" $
            parse moduleStatement "" "module Main exposing (Maybe(Just, Nothing), Either(..), main)" `shouldParse`
                ModuleStatement "Main"
                    [ ModuleType "Maybe" [TypeSymbol "Just", TypeSymbol "Nothing"]
                    , ModuleType "Either" [AllVariants]
                    , ModuleFunction "main"
                    ]

    describe "importStatement" $ do
        it "parses a simple import statement" $
            parse importStatement "" "import Html" `shouldParse`
                ImportStatement "Html" []

        it "parses a deep import" $
            parse importStatement "" "import Html.Events" `shouldParse`
                ImportStatement "Html.Events" []

        it "parses an open import statement with all symbols" $
            parse importStatement "" "import Html exposing (..)" `shouldParse`
                ImportStatement "Html" [AllSymbols]

        it "parses an open import statement with particular symbols" $
            parse importStatement "" "import Maybe exposing (Maybe (..), isJust)" `shouldParse`
                ImportStatement "Maybe"
                    [ ModuleType "Maybe" [AllVariants]
                    , ModuleFunction "isJust"
                    ]

        it "parses a qualified import" $
            parse importStatement "" "import Html as H" `shouldParse`
                QualifiedImportStatement "Html" "H" []

        it "parses a qualified open import" $
            parse importStatement "" "import Html as H exposing (..)" `shouldParse`
                QualifiedImportStatement "Html" "H" [AllSymbols]

    describe "topLevelProgram" $ do
        it "parses a simple program" $
            let
                result =
                    Program
                        Nothing
                        [ ImportDecl (ImportStatement "Html" [AllSymbols])
                        , FunctionDecl
                            (BoundFunctionDefinition
                                Nothing
                                "main"
                                []
                                (FunctionApplication (Variable "text") (String "Hello"))
                            )
                        ]
             in do
                parse topLevelProgram "" topLevelProgramSimpleA `shouldParse` result
                parse topLevelProgram "" topLevelProgramSimpleB `shouldParse` result
                parse topLevelProgram "" topLevelProgramSimpleC `shouldParse` result

        it "parses multiple imports" $
            let
                result =
                    Program
                        Nothing
                        [ ImportDecl (ImportStatement "Html" [AllSymbols])
                        , ImportDecl (ImportStatement "Browser" [])
                        , FunctionDecl
                            (BoundFunctionDefinition
                                Nothing
                                "main"
                                []
                                (FunctionApplication (Variable "text") (String "Hello"))
                            )
                        ]
             in
                parse topLevelProgram "A" topLevelProgramMultipleImportsA `shouldParse` result

        it "parses multiple imports with closed, unqualified import first" $
            let
                result =
                    Program
                        Nothing
                        [ ImportDecl (ImportStatement "Browser" [])
                        , ImportDecl (ImportStatement "Html" [AllSymbols])
                        , FunctionDecl
                            (BoundFunctionDefinition
                                Nothing
                                "main"
                                []
                                (FunctionApplication (Variable "text") (String "Hello"))
                            )
                        ]
             in
                parse topLevelProgram "B" topLevelProgramMultipleImportsB `shouldParse` result

        it "parses a simple program with module" $
            let
                result =
                    Program
                        (Just (ModuleStatement "Simple" [AllSymbols]))
                        [ ImportDecl (ImportStatement "Html" [AllSymbols])
                        , FunctionDecl
                            (BoundFunctionDefinition
                                (Just
                                    (Type "Html" [TypeArg "a"])
                                )
                                "main"
                                []
                                (FunctionApplication (Variable "text") (String "Hello"))
                            )
                        ]

            in
                testParserWithFile topLevelProgram "Simple.elm" result

        it "parses a simple program with custom types and aliases" $
            testParserWithFile topLevelProgram "TopLevels.elm" $
                Program
                    (Just (ModuleStatement "TopLevels" [AllSymbols]))
                    [ ImportDecl (ImportStatement "Html" [])
                    , TypeDecl
                        ( TypeConstructorDefinition "Msg" []
                            [ Variant "Inc" []
                            , Variant "Dec" []
                            ]
                        )
                    , TypeAliasDecl
                        ( TypeAlias "Point"
                            ( TupleType
                                [ Type "Int" []
                                , Type "Int" []
                                ]
                            )
                        )
                    , FunctionDecl
                        ( BoundFunctionDefinition
                            ( Just
                                ( Type "Html" [ TypeArg "a" ] )
                            ) "main" []
                            ( FunctionApplication ( Variable "text" ) ( String "Hello" ) )
                        )
                    ]

        it "parses comments preceding program" $
            let
                result =
                    Program
                        Nothing
                        [ FunctionDecl
                            (BoundFunctionDefinition
                                Nothing
                                "main"
                                []
                                (FunctionApplication (Variable "text") (String "Hello"))
                            )
                        ]
            in
                parse topLevelProgram "" topLevelProgramPrecedingComments `shouldParse` result

        it "fails on whitespace before top-level import" $
            parse topLevelProgram "" `shouldFailOn` topLevelImportWhitespaceFailure

        it "fails on whitespace before top-level module" $
            parse topLevelProgram "" `shouldFailOn` topLevelModuleWhitespaceFailure

        it "fails on whitespace before top-level function declarations" $
            parse topLevelProgram "" `shouldFailOn` topLevelFunctionWhitespaceFailure

        it "fails on expression in the same column as declaration" $
            parse topLevelProgram "" `shouldFailOn` topLevelFunctionColumnFailure

        it "TODO parses case expressions" $
            testParserWithFile topLevelProgram "Cases.elm" $
                Program
                    (Just (ModuleStatement "Cases" [AllSymbols]))
                    [ TypeDecl
                        ( TypeConstructorDefinition "Msg" []
                            [ Variant "Inc" []
                            , Variant "Dec" []
                            ]
                        )
                    , FunctionDecl
                        ( BoundFunctionDefinition
                            Nothing
                            "update"
                            [PatternVariable "message", PatternVariable "model"]
                            (Case
                                (Variable "message")
                                [ CaseBranch (PatternConstructor "Inc" []) (Int 1)
                                , CaseBranch (PatternConstructor "Dec" []) (Int 0)
                                ]
                            )
                        )
                    , FunctionDecl
                        (BoundFunctionDefinition
                            Nothing
                            "main"
                            []
                            (FunctionApplication (Variable "text") (String "Hello"))
                        )
                    ]
