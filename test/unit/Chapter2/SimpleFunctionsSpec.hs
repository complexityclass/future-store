module Chapter2.SimpleFunctionsSpec where

import Test.Hspec
import Chapter2.SimpleFunctions

spec :: Spec
spec = do
    describe "Prelude.head" $ do

        it "reverse the string" $ do
            reverse2 "hello" `shouldBe` "olleh"