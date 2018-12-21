module InferenceSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Test.Hspec
import Text.Megaparsec

import AST
import Inference
import Lib
import TestUtility


spec :: Spec
spec = do
    describe "applyUnifier" $ do
        it "substitutes a type argument" $
            apply (Map.singleton "a" (Type "Char" [])) (TypeArg "a") `shouldBe`
                Type "Char" []

        it "empty substitution acts as identity" $
            apply Map.empty (TypeArg "a") `shouldBe` TypeArg "a"

        it "substitution does nothing without type arguments" $
            apply (Map.singleton "a" (Type "Char" [])) (Type "Int" []) `shouldBe`
                Type "Int" []

        it "substitutes a function type" $
            let
                u = Map.fromList
                    [ ("a", Type "Char" [])
                    , ("b", Type "Bool" [])
                    ]
            in
                apply u (TypeFunc (TypeArg "a") (TypeArg "b")) `shouldBe`
                    TypeFunc (Type "Char" []) (Type "Bool" [])

    describe "unify" $ do
        it "two identical type arguments are already unified" $
            unify (TypeArg "a") (TypeArg "a") `shouldBe` Map.empty

        it "unifies two type arguments" $
            unify (TypeArg "a") (TypeArg "b") `shouldBe` Map.singleton "a" (TypeArg "b")

        it "unifies a left type argument" $
            unify (TypeArg "a") (Type "Char" []) `shouldBe` Map.singleton "a" (Type "Char" [])

        it "unifies a right type argument" $
            unify (Type "Char" []) (TypeArg "a") `shouldBe` Map.singleton "a" (Type "Char" [])

        it "unifies two function types" $
            unify (TypeFunc (TypeArg "a") (TypeArg "b")) (TypeFunc (TypeArg "c") (TypeArg "d")) `shouldBe`
                Map.fromList
                    [ ("a", TypeArg "c")
                    , ("b", TypeArg "d")
                    ]

        it "unifies two function types with a concrete type" $
            unify (TypeFunc (TypeArg "a") (TypeArg "b")) (TypeFunc (TypeArg "c") (Type "Char" [])) `shouldBe`
                Map.fromList
                    [ ("a", TypeArg "c")
                    , ("b", Type "Char" [])
                    ]

        it "throws an error unifying a function type and concrete type" $
            evaluate (unify (TypeFunc (TypeArg "a") (TypeArg "b")) (Type "Char" [])) `shouldThrow` anyException

        it "unifies a type argument and a function type" $
            unify (TypeArg "a") (TypeFunc (TypeArg "b") (TypeArg "c")) `shouldBe`
                Map.singleton "a" (TypeFunc (TypeArg "b") (TypeArg "c"))

        it "throws an error unifying different constructors" $
            evaluate (unify (Type "Char" []) (Type "Bool" [])) `shouldThrow`
                errorCall differentConstructorsErrorMessage

        it "unifies two constructors" $
            unify (Type "List" [TypeArg "a"]) (Type "List" [Type "Char" []]) `shouldBe`
                Map.singleton "a" (Type "Char" [])

        it "does need to unify two constructors with the same type args" $
            unify (Type "List" [TypeArg "a"]) (Type "List" [TypeArg "a"]) `shouldBe`
                Map.empty

        it "unifies two constructors with multiple parameters" $
            unify (Type "Either" [TypeArg "a", Type "Bool" []]) (Type "Either" [Type "Char" [], TypeArg "b"]) `shouldBe`
                Map.fromList
                    [ ("a", Type "Char" [])
                    , ("b", Type "Bool" [])
                    ]

        it "unifies complex type function 1" $
            unify (TypeFunc (TypeArg "a") (Type "Char" [])) (TypeFunc (Type "List" [TypeArg "b"]) (TypeArg "b")) `shouldBe`
                Map.fromList
                    [ ("a", Type "List" [Type "Char" []])
                    , ("b", Type "Char" [])
                    ]

        it "unifies complex type function 2" $
            unify (TypeFunc (TypeArg "a") (Type "List" [TypeArg "c"])) (TypeFunc (TypeArg "b") (TypeArg "a")) `shouldBe`
                Map.fromList
                    [ ("a", Type "List" [TypeArg "c"])
                    , ("b", Type "List" [TypeArg "c"])
                    ]

        it "unifies complex type function 3" $
            unify (Type "Either" [TypeArg "a", TypeArg "b"]) (Type "Either" [TypeArg "b", TypeArg "a"]) `shouldBe`
                Map.fromList
                    [ ("a", TypeArg "b")
                    , ("b", TypeArg "b")
                    ]

        it "throws an error on an infinite type" $ do
            evaluate (unify (TypeArg "a") (Type "List" [TypeArg "a"])) `shouldThrow`
                errorCall infiniteTypeErrorMessage

            unify (TypeArg "b") (Type "List" [TypeArg "a"]) `shouldBe`
                Map.singleton "b" (Type "List" [TypeArg "a"])

    describe "infer" $ do
        it "infers the identity function" $
            evalState (infer emptyDefinitions emptyEnvironment (AnonymousFunction [PatternVariable "x"] (Variable "x"))) 0 `shouldBe`
                ( Map.empty
                , TypeFunc (TypeArg "t0") (TypeArg "t0")
                )

        it "infers an anything pattern parameter" $
            parseInfer "(\\_ -> 'a')" `shouldBe`
                TypeFunc (TypeArg "t0") (Type "Char" [])

        it "infers a string pattern parameter" $
            parseInfer "(\\\"hello\" -> 'a')" `shouldBe`
                TypeFunc (Type "String" []) (Type "Char" [])

        it "infers a bool pattern parameter" $
            parseInfer "(\\True -> 'a')" `shouldBe`
                TypeFunc (Type "Bool" []) (Type "Char" [])

        it "infers function application" $
            let
                expr =
                    (FunctionApplication
                        (AnonymousFunction [PatternVariable "x"] (Variable "x"))
                        (Bool True)
                    )
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    Type "Bool" []

        it "infers the left const function" $
            let
                expr =
                    (FunctionApplication
                        (FunctionApplication
                            (AnonymousFunction
                                [PatternVariable "x"]
                                (AnonymousFunction
                                    [PatternVariable "y"]
                                    (Variable "x")
                                )
                            )
                            (Bool True)
                        )
                        (Char 'a')
                    )
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    Type "Bool" []

        it "infers the right const function" $
            let
                expr =
                    (FunctionApplication
                        (FunctionApplication
                            (AnonymousFunction
                                [PatternVariable "x"]
                                (AnonymousFunction
                                    [PatternVariable "y"]
                                    (Variable "y")
                                )
                            )
                            (Bool True)
                        )
                        (Char 'a')
                    )
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    Type "Char" []

        it "infers a let binding" $
            let
                expr =
                    (LetBinding
                        [ BoundFunctionDefinition
                            Nothing
                            "x"
                            [PatternVariable "y"]
                            (Variable "y")
                        ]
                        (FunctionApplication (Variable "x") (Bool True))
                    )
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    Type "Bool" []

        it "handles let polymorphism" $
            let
                f =
                    BoundFunctionDefinition
                        Nothing
                        "f"
                        [PatternVariable "x"]
                        (Variable "x")

                g =
                    BoundFunctionDefinition
                        Nothing
                        "g"
                        [PatternVariable "y"]
                        (FunctionApplication (Variable "f") (Char 'a'))

                expr =
                    (LetBinding
                        [f]
                        (LetBinding
                            [g]
                            (FunctionApplication (Variable "f") (Bool True))
                        )
                    )
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    Type "Bool" []

        it "handles nullary functions" $
            let
                expr = AnonymousFunction [] (Bool True)
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    Type "Bool" []

        it "handles functions with arity > 1" $
            let
                expr = AnonymousFunction (PatternVariable <$> ["x", "y", "z"]) (Bool True)
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    TypeFunc
                        (TypeArg "t0")
                        (TypeFunc
                            (TypeArg "t1")
                            (TypeFunc
                                (TypeArg "t2")
                                (Type "Bool" [])
                            )
                        )

        it "handles function application of arity > 1 (right arg)" $
            parseInfer "(\\x y z -> z) True \"hello\" 'a'" `shouldBe`
                Type "Char" []

        it "handles function application of arity > 1 (middle arg)" $
            parseInfer "(\\x y z -> y) True \"hello\" 'a'" `shouldBe`
                Type "String" []

        it "handles function application of arity > 1 (left arg)" $
            parseInfer "(\\x y z -> x) True \"hello\" 'a'" `shouldBe`
                Type "Bool" []

        it "handles a let binding declaration with multiple patterns" $
            parseInfer "let f x y = y in f True 'a'" `shouldBe`
                Type "Char" []

        it "handles multiple declarations in a let binding" $
            let
                f =
                    BoundFunctionDefinition
                        Nothing
                        "f"
                        [PatternVariable "x"]
                        (Variable "x")

                g =
                    BoundFunctionDefinition
                        Nothing
                        "g"
                        [PatternVariable "y"]
                        (FunctionApplication (Variable "f") (Char 'a'))

                expr =
                    (LetBinding
                        [f, g]
                        (FunctionApplication (Variable "f") (Bool True))
                    )
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    Type "Bool" []

        it "infers an empty list" $
            snd (evalState (infer emptyDefinitions emptyEnvironment (List [])) 0) `shouldBe`
                Type "List" [TypeArg "t0"]

        it "infers a list" $
            snd (evalState (infer emptyDefinitions emptyEnvironment (List [Bool True, Bool False])) 0) `shouldBe`
                Type "List" [Type "Bool" []]

        it "infers a tuple" $
            let
                expr =
                    Tuple
                        [ Bool True
                        , String "hello"
                        , Char 'a'
                        ]
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    TupleType
                        [ Type "Bool" []
                        , Type "String" []
                        , Type "Char" []
                        ]

        it "infers a conditional" $
            let
                expr =
                    If
                        (Bool True)
                        (Bool True)
                        (Bool False)
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    Type "Bool" []

        it "infers a bare nullary data constructor/variant" $
            let
                expr =
                    Constructor "Nothing"
            in
                snd (evalState (infer standardDefinitions emptyEnvironment expr) 0) `shouldBe`
                    Type "Maybe" [TypeArg "t0"]

        it "infers a bare nullary data constructor/variant" $
            let
                expr =
                    Constructor "Just"
            in
                snd (evalState (infer standardDefinitions emptyEnvironment expr) 0) `shouldBe`
                    TypeFunc (TypeArg "t0") (Type "Maybe" [TypeArg "t0"])

        it "infers a application data constructor/variant" $
            let
                expr =
                    FunctionApplication
                        (Constructor "Just")
                        (Bool True)
            in
                snd (evalState (infer standardDefinitions emptyEnvironment expr) 0) `shouldBe`
                    Type "Maybe" [Type "Bool" []]

        it "infers a simple constructor pattern without type arguments" $
            let
                definitions =
                    constructDefinitions
                        [ TypeConstructorDefinition
                            "Bar"
                            []
                            [Variant "Foo" []]
                        ]

                expr =
                    AnonymousFunction
                        [PatternConstructor "Foo" []]
                        (Char 'a')
            in
                snd (evalState (infer definitions emptyEnvironment expr) 0) `shouldBe`
                    TypeFunc (Type "Bar" []) (Type "Char" [])

        it "infers a flat tuple pattern" $
            -- f : (a, b, c) -> (a, b)
            -- f (a, b, _) = (a, b)
            let
                expr =
                    AnonymousFunction
                        [PatternTuple
                            [ PatternVariable "a"
                            , PatternVariable "b"
                            , PatternAnything
                            ]
                        ]
                        (Tuple [Variable "a", Variable "b"])
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    TypeFunc
                        (TupleType
                            [ TypeArg "t0"
                            , TypeArg "t1"
                            , TypeArg "t2"
                            ]
                        )
                        (TupleType
                            [ TypeArg "t0"
                            , TypeArg "t1"
                            ]
                        )

        it "does not re-instantiate type arguments with prior usage" $
            -- f (_, a, a) = (a, a)
            let
                expr =
                    AnonymousFunction
                        [PatternTuple
                            [ PatternAnything
                            , PatternVariable "a"
                            , PatternVariable "a"
                            ]
                        ]
                        (Tuple [Variable "a", Variable "a"])
            in
                snd (evalState (infer emptyDefinitions emptyEnvironment expr) 0) `shouldBe`
                    TypeFunc
                        (TupleType
                            [ TypeArg "t0"
                            , TypeArg "t1"
                            , TypeArg "t1"
                            ]
                        )
                        (TupleType
                            [ TypeArg "t1"
                            , TypeArg "t1"
                            ]
                        )

        it "infers a non-nested constructor pattern" $
            -- f : Maybe a -> a
            -- f (Just a) = a
            let
                expr =
                    AnonymousFunction
                        [PatternConstructor "Just" [PatternVariable "a"]]
                        (Variable "a")
            in
                snd (evalState (infer standardDefinitions emptyEnvironment expr) 0) `shouldBe`
                    TypeFunc
                        (Type "Maybe" [TypeArg "t0"])
                        (TypeArg "t0")


        it "infers a complex constructor pattern" $
            -- f : Either (Maybe a) b -> a
            -- f (Left (Just a)) = a
            let
                expr =
                    AnonymousFunction
                        [PatternConstructor "Left"
                            [PatternConstructor "Just" [PatternVariable "a"]]
                        ]
                        (Variable "a")
            in
                snd (evalState (infer standardDefinitions emptyEnvironment expr) 0) `shouldBe`
                    TypeFunc
                        (Type "Either" [Type "Maybe" [TypeArg "t2"], TypeArg "t1"])
                        (TypeArg "t2")

        it "infers a complex constructor pattern with multiple pattern arguments" $
            -- f : State (Maybe a) (Maybe b) -> (b, a)
            -- f (State (Just a) (Just b)) = (b, a)
            let
                expr =
                    AnonymousFunction
                        [PatternConstructor "State"
                            [ PatternConstructor "Just" [PatternVariable "a"]
                            , PatternConstructor "Just" [PatternVariable "b"]
                            ]
                        ]
                        (Tuple [Variable "b", Variable "a"])

                definitions =
                    constructDefinitions
                        [ maybeDef
                        , TypeConstructorDefinition
                            "State"
                            [TypeConstructorArg "a", TypeConstructorArg "b"]
                            [Variant "State" [TypeArg "a", TypeArg "b"]]
                        ]
            in
                snd (evalState (infer definitions emptyEnvironment expr) 0) `shouldBe`
                    TypeFunc
                        (Type
                            "State"
                            [Type "Maybe" [TypeArg "t2"], Type "Maybe" [TypeArg "t4"]]
                        )
                        (TupleType [TypeArg "t4", TypeArg "t2"])

        it "infers a record value" $
            parseInfer "{ x = 'a', y = True }" `shouldBe`
                RecordType (Map.fromList
                    [ ("x", Type "Char" [])
                    , ("y", Type "Bool" [])
                    ])

        it "infers a record update" $
            parseInfer "let foo = { x = 'a', y = True } in { foo | y = False }" `shouldBe`
                RecordType (Map.fromList
                    [ ("x", Type "Char" [])
                    , ("y", Type "Bool" [])
                    ])

        it "infers a case expression" $
            parseInfer "case x of True -> 'a'" `shouldBe`
                Type "Char" []

        it "infers negate from elm/core/Basics" $
            parseInfer "negate" `shouldBe`
                TypeFunc (ConstrainedTypeVariable Number) (ConstrainedTypeVariable Number)

        it "infers negate application" $ do
            parseInfer "-4.5" `shouldBe` Type "Float" []
            parseInfer "-4" `shouldBe` ConstrainedTypeVariable Number

        it "infers append application" $ do
            parseInfer "(\\x -> x ++ x)" `shouldBe`
                TypeFunc
                    (ConstrainedTypeVariable Appendable)
                    (ConstrainedTypeVariable Appendable)

    describe "constructDefinitions" $
        it "constructs a correct map" $
            standardDefinitions `shouldBe`
                Definitions
                    (Map.fromList
                        [ ("Nothing", (nothing, maybeDef))
                        , ("Just", (just, maybeDef))
                        , ("Left", (left, eitherDef))
                        , ("Right", (right, eitherDef))
                        ])

    describe "constructorType" $ do
        it "handles simple constructor" $
            let
                typeVarMap =
                    Map.singleton "a" (TypeArg "t0")

                resultType =
                    Type "Foo" [TypeArg "t0"]

                variantParameters =
                    [ TypeArg "a" ]
            in
                constructorType typeVarMap resultType variantParameters `shouldBe`
                    TypeFunc (TypeArg "t0") resultType

        it "handles nested data constructor" $
            let
                typeVarMap =
                    Map.fromList
                        [ ("a", TypeArg "t0")
                        , ("b", TypeArg "t1")
                        ]

                resultType =
                    Type "Foo" [TypeArg "t0", TypeArg "t1"]

                -- type Foo a b = Bar (Either (Maybe a) Bool) (Maybe b)
                variantParameters =
                    [ Type "Either" [Type "Maybe" [TypeArg "a"], Type "Bool" []]
                    , Type "Maybe" [TypeArg "b"]
                    ]
            in
                constructorType typeVarMap resultType variantParameters `shouldBe`
                    TypeFunc
                        (Type "Either" [Type "Maybe" [TypeArg "t0"], Type "Bool" []])
                        (TypeFunc
                            (Type "Maybe" [TypeArg "t1"])
                            resultType
                        )
