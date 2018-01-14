module Chapter4.Containers (
    insertF,
    deleteF,
    adjustF,
    preOrder
    ) where

import qualified Data.Map as M
import Data.Tree

insertF :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
insertF key value = M.alter (\(Nothing) -> (Just value)) key

deleteF :: (Ord k) => k -> M.Map k a -> M.Map k a
deleteF key = M.alter (\(Just _) -> Nothing) key

adjustF :: (Ord k) => (a -> a) -> k -> M.Map k a -> M.Map k a
adjustF transform key = M.alter (\value -> case value of
                                           Nothing -> Nothing
                                           (Just value) -> Just (transform value)) key

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subtrees) = 
    let subtreesTraversed = concat $ map (preOrder f) subtrees
     in f v : subtreesTraversed
