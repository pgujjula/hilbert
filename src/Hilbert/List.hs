{-|
Module      : Hilbert.List
Description : Utility functions to handle lists.
Copyright   : (c) Preetham Gujjula, 2016
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental

Utility functions to handle lists.
-}

module Hilbert.List
  ( rmDups
  , groupBy
  ) where

import Data.List (sortBy)

{-| @rmDups xs@ removes the duplicates from a list @xs@.

    > Precondition: xs must be sorted

    >>> rmDups [1, 1, 2, 4, 6, 6, 6, 6, 10]
    [1, 2, 4, 6, 10]
-}
rmDups :: (Eq a) => [a] -> [a]
rmDups (x1:x2:xs) = if x1 == x2
                    then rmDups (x1:xs)
                    else x1:(rmDups (x2:xs))
rmDups xs = xs

{-| Group elements of a list using a comparison function.
 
    > Preconditions: none

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
