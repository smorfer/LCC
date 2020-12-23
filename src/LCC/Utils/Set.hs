module LCC.Utils.Set (symmetricDifference, setList) where

import           Data.Set (fromList, toList, Set, union, difference
                         , intersection)

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference a b = (a `union` b) `difference` (a `intersection` b)

setList :: (Ord a) => [a] -> [a]
setList = toList . fromList
