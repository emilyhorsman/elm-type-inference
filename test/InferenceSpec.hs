module InferenceSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Test.Hspec

import AST
import Inference


spec :: Spec
spec = do
    describe "applyUnifier" $ do
        it "substitutes a type argument" $
            applyUnifier (Map.singleton "a" (Type "Char" [])) (TypeArg "a") `shouldBe`
                Type "Char" []

        it "empty substitution acts as identity" $
            applyUnifier Map.empty (TypeArg "a") `shouldBe` TypeArg "a"

        it "substitution does nothing without type arguments" $
            applyUnifier (Map.singleton "a" (Type "Char" [])) (Type "Int" []) `shouldBe`
                Type "Int" []

        it "substitutes a function type" $
            let
                u = Map.fromList
                    [ ("a", Type "Char" [])
                    , ("b", Type "Bool" [])
                    ]
            in
                applyUnifier u (TypeFunc (TypeArg "a") (TypeArg "b")) `shouldBe`
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
            evalState (infer Map.empty (AnonymousFunction ["x"] (Variable "x"))) 0 `shouldBe`
                ( Map.empty
                , TypeFunc (TypeArg "t0") (TypeArg "t0")
                )

        it "infers with assumption requiring instantiation" $
            let
                assumptions =
                    (Map.singleton "y" (TypeArg "t0"))

                expression =
                    (AnonymousFunction ["x"] (Variable "y"))
            in
                evalState (infer assumptions expression) 0 `shouldBe`
                    ( Map.empty
                    , TypeFunc (TypeArg "t0") (TypeArg "t1")
                    )
