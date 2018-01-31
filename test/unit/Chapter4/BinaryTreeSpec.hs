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
             
        it "Tree traverse" $ do
            let tree = Node 7 (Node 4 (Node 2 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 11 (Node 8 Leaf Leaf) (Node 21 Leaf Leaf))
             in (treeTraverse tree) `shouldBe` [2, 4, 5, 7, 8, 11, 21]

        it "Tree union" $ do
            let tree1 = Node 7 (Node 4 (Node 2 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 11 (Node 8 Leaf Leaf) (Node 21 Leaf Leaf))
                tree2 = Node 8 (Node 5 (Node 3 Leaf Leaf) (Node 6 Leaf Leaf)) (Node 12 (Node 9 Leaf Leaf) (Node 22 Leaf Leaf))
                union = treeUnion tree1 tree2
             in (treeTraverse union) `shouldBe` [2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 21, 22]
