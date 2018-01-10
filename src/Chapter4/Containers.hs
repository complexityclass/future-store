module Chapter4.Containers (
    insertF
    ) where

import qualified Data.Map as M

insertF :: (Ord k) => k -> a -> M.Map k a -> M.Map k a
insertF key value = M.alter (\(Nothing) -> (Just value)) key 

