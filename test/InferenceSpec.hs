module InferenceSpec (spec) where

import Test.Hspec

import AST
import Inference


spec :: Spec
spec = do
    describe "expressionInference" $ do
        it "infers chars" $
            expressionInference [] [] (Char 'a') `shouldBe` Type "Char" []

        it "infers numbers" $
            expressionInference [] [] (Int 1) `shouldBe` ConstrainedTypeVariable Number

        it "infers floats" $
            expressionInference [] [] (Float 1) `shouldBe` Type "Float" []
