module App.Utils.CachedBinaryTree (
    CachedBinaryTree (..),
    --treeInsert,
    --treeFind,
    --treeTraverse,
    --treeUnion
) where

import Data.Monoid

data CachedBinaryTree v c = Node v c (CachedBinaryTree v c) (CachedBinaryTree v c)
                          | Leaf
                          deriving (Show, Eq, Ord)

treeInsert :: (Ord v, Monoid c) => v -> c -> CachedBinaryTree v c -> CachedBinaryTree v c
treeInsert v c (Node v2 c2 l r) = case compare v v2 of
    EQ -> Node v2 c2 l r
    LT -> let newLeft = treeInsert v c l
              newCache = c2 <> cached newLeft <> cached r
           in Node v2 newCache newLeft r
    GT -> let newRight = treeInsert v c r
              newCache = c2 <> cached l <> cached newRight
           in Node v2 newCache l newRight
treeInsert v c Leaf = Node v c Leaf Leaf

cached :: Monoid c => CachedBinaryTree v c -> c
cached (Node _ c _ _) = c
cached Leaf           = mempty
