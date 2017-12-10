module Chapter2.SimpleFunctionsSpec where

import Test.Hspec
import Chapter2.SimpleFunctions

spec :: Spec
spec = do
    describe "Prelude.head" $ do

        it "reverse the string" $ do
            reverse2 "hello" `shouldBe` "olleh"

        it "concat two strings" $ do
            "hello" +++ " world" `shouldBe` "hello world"
        
        it "find max and min" $ do
            maxmin [3,4,1,9,6] `shouldBe` (9, 1)