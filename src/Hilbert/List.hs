module Hilbert.List
  ( rmDups
  , rmDupsWith
  , groupBy
  ) where

import Data.List (sortBy)

{-| Removes duplicates from a sorted list.
    Undefined behavior on nonsorted lists.

  >>> rmDups [1, 1, 2, 4, 6, 6, 6, 6, 10]
  [1, 2, 4, 6, 10]
-}
rmDups :: (Eq a) => [a] -> [a]
rmDups (x1:x2:xs) = if x1 == x2
                    then rmDups (x1:xs)
                    else x1:(rmDups (x2:xs))
rmDups xs = xs

{-| Removes duplicates from a sorted list,
    using a custom comparison function. Undefined
    behavior on nonsorted lists. -}
rmDupsWith :: (a -> a -> Bool) -> [a] -> [a]
rmDupsWith func (x1:x2:xs) = if func x1 x2
                             then rmDupsWith func (x1:xs)
                             else x1:(rmDupsWith func (x2:xs))
rmDupsWith func xs = xs

{-| Group elements of a list using a comparison function.
 
>>> groupBy compare [4, 3, 5, 6, 7, 8, 3, 6, 8, 5, 3]
[[3,3,3],[4],[5,5],[6,6],[7],[8,8]]
 -}
groupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupBy func list = groupAscending func $ sortBy func list
  where
    groupAscending :: (a -> a -> Ordering) -> [a] -> [[a]]
    groupAscending _ [] = []
    groupAscending _ [x] = [[x]]
    groupAscending func (x:xs) = if (func x (head box)) == EQ
                                 then (x:box):boxes
                                 else [x]:(box:boxes)
                              where (box:boxes) = groupAscending func xs
