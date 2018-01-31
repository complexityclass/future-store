module App.Utils.BinaryTree (
    BinaryTree (..),
    treeInsert,
    treeFind,
    treeTraverse,
    treeUnion
) where


data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                | Leaf
                deriving Show


treeFind :: Ord a => a -> BinaryTree a -> Maybe a
treeFind t (Node v l r) = case compare t v of
    EQ -> Just v
    LT -> treeFind t l
    GT -> treeFind t r
treeFind _ Leaf = Nothing


treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert t n@(Node v l r) = case compare t v of
    EQ -> n
    LT -> Node v (treeInsert t l) r
    GT -> Node v l (treeInsert t r)
treeInsert t Leaf = Node t Leaf Leaf

treeTraverse :: Ord a => BinaryTree a -> [a]
treeTraverse Leaf = []
treeTraverse (Node v l r) = (treeTraverse l) ++ [v] ++ (treeTraverse r)

treeUnion :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeUnion lTree rTree = append (treeTraverse lTree) rTree 
    where 
        append [] tree = tree
        append (x:xs) tree = append xs (treeInsert x tree)

