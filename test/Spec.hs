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
            parse singleChar "" "'\\n'" `shouldParse` '\n'
            parse singleChar "" "'\\r'" `shouldParse` '\r'
            parse singleChar "" "'\\t'" `shouldParse` '\t'
            parse singleChar "" "'\\\"'" `shouldParse` '"'
            parse singleChar "" "'\\''" `shouldParse` '\''
            parse singleChar "" "'\\\\'" `shouldParse` '\\'

        it "returns char" $ do
            parse singleChar "" "'a'" `shouldParse` 'a'

        it "fails on invalid escape sequence" $ do
            parse singleChar "" `shouldFailOn` "'\\a'"

        it "fails on unescaped quote" $ do
            parse singleChar "" `shouldFailOn` "'''"
