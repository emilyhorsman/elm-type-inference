module InferenceSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import AST
import Inference


spec :: Spec
spec = do
    describe "applySubstitution" $ do
        it "substitutes a type argument" $
            applySubstitution (Map.singleton "a" (Type "Char" [])) (TypeArg "a") `shouldBe`
                Type "Char" []

        it "empty substitution acts as identity" $
            applySubstitution Map.empty (TypeArg "a") `shouldBe` TypeArg "a"

        it "substitution does nothing without type arguments" $
            applySubstitution (Map.singleton "a" (Type "Char" [])) (Type "Int" []) `shouldBe`
                Type "Int" []
