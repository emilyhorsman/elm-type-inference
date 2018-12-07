module InferenceSpec (spec) where

import Test.Hspec


spec :: Spec
spec = do
    describe "inference" $ do
        it "test" $
            True `shouldBe` True
