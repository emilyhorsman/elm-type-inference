import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import Lib


main :: IO ()
main = hspec $ do
    describe "bool" $ do
        it "returns True" $ do
            parse bool "" "True" `shouldParse` True

        it "returns False" $ do
            parse bool "" "False" `shouldParse` False

        it "fails" $ do
            parse bool "" `shouldFailOn` "asdf"

    describe "singleChar" $ do
        it "returns escaped char" $ do
            parse singleChar "" "'\''" `shouldParse` '\''

        it "returns char" $ do
            parse singleChar "" "'a'" `shouldParse` 'a'

        it "fails on unescaped quote" $ do
            parse singleChar "" `shouldFailOn` "'''"
