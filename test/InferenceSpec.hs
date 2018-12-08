module InferenceSpec (spec) where

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
                u = Map.fromList $
                    [ ("a", Type "Char" [])
                    , ("b", Type "Bool" [])
                    ]
            in
                applyUnifier u (TypeFunc (TypeArg "a") (TypeArg "b")) `shouldBe`
                    TypeFunc (Type "Char" []) (Type "Bool" [])
