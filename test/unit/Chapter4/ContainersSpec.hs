module Chapter4.ContainersSpec where
    
import Test.Hspec
import Chapter4.Containers
import Data.Char
import qualified Data.Map as M

spec :: Spec
spec = do
    describe "Prelude.head" $ do

        it "Insert in map" $ do
            (insertF "key2" "value2" (M.singleton "key1" "value1")) 
            `shouldBe` (M.fromList [("key1", "value1"), ("key2", "value2")])

        it "Delete in map" $ do
            (deleteF "key2" (M.fromList [("key1", "value1"), ("key2", "value2")]))
            `shouldBe` (M.singleton "key1" "value1")
            
        it "Adjust existing value in map" $ do
            (adjustF (\str -> map toUpper str) "key2" (M.fromList [("key1", "value1"), ("key2", "value2")]))
            `shouldBe` (M.fromList [("key1", "value1"), ("key2", "VALUE2")])

        it "Adjust missed value in map" $ do
            (adjustF (\str -> map toUpper str) "key3" (M.fromList [("key1", "value1"), ("key2", "value2")]))
            `shouldBe` (M.fromList [("key1", "value1"), ("key2", "value2")])
