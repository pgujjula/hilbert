module Hilbert.List
  ( rmDups
  , rmDupsWith
  , groupBy
  ) where

import Data.List (sortBy)

-- Removes duplicates from a sorted list
rmDups :: (Eq a) => [a] -> [a]
rmDups (x1:x2:xs) = if x1 == x2
                    then rmDups (x1:xs)
                    else x1:(rmDups (x2:xs))
rmDups xs = xs

-- Removes duplicates from a sorted list, 
-- using a custom comparison function
rmDupsWith :: (a -> a -> Bool) -> [a] -> [a]
rmDupsWith func (x1:x2:xs) = if func x1 x2
                             then rmDupsWith func (x1:xs)
                             else x1:(rmDupsWith func (x2:xs))
rmDupsWith func xs = xs

-- Group elements of a list using a comparison function
groupBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupBy func list = groupAscending func $ sortBy func list

groupAscending :: (a -> a -> Ordering) -> [a] -> [[a]]
groupAscending _ [] = []
groupAscending _ [x] = [[x]]
groupAscending func (x:xs) = if (func x (head box)) == EQ
                             then (x:box):boxes
                             else [x]:(box:boxes)
                          where (box:boxes) = groupAscending func xs
