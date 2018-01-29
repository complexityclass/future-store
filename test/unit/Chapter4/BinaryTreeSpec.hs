module Chapter4.BinaryTreeSpec where

import Test.Hspec
import App.Utils.BinaryTree

spec :: Spec
spec = do
    describe "Prelude.head" $ do
        
        it "Tree find existed" $ do
            let tree = Node 7 (Node 4 (Node 2 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 11 (Node 8 Leaf Leaf) (Node 21 Leaf Leaf))
             in (treeFind 5 tree) `shouldBe` (Just 5)
             
        it "Tree find missed" $ do
           let tree = Node 7 (Node 4 (Node 2 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 11 (Node 8 Leaf Leaf) (Node 21 Leaf Leaf))
            in (treeFind 17 tree) `shouldBe` Nothing

        it "Tree insert" $ do
            let tree = Node 7 (Node 4 (Node 2 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 11 (Node 8 Leaf Leaf) (Node 21 Leaf Leaf))
                afterInsert = treeInsert 12 tree
             in (treeFind 12 afterInsert) `shouldBe` (Just 12)   