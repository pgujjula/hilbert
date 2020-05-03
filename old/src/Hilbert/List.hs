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
  , mergeMany
  ) where

import Prelude hiding (null)
import Data.List (sortBy)
import qualified Data.List as List (null)

import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQueue

{-|
    @rmDups xs@ removes the duplicates from a list @xs@.

    __Precondition:__ @xs@ must be sorted.

    >>> rmDups [1, 1, 2, 4, 6, 6, 6, 6, 10]
    [1, 2, 4, 6, 10]
-}
rmDups :: (Eq a) => [a] -> [a]
rmDups (x1:x2:xs) = if x1 == x2
                    then rmDups (x1:xs)
                    else x1:(rmDups (x2:xs))
rmDups xs = xs

{-|
    Group elements of a list using a comparison function.
    
    __Preconditions:__ None.

    To group equal elements,

    >>> groupBy compare [4, 3, 5, 6, 7, 8, 3, 6, 8, 5, 3]
    [[3,3,3],[4],[5,5],[6,6],[7],[8,8]]

    To group elements by their residue modulo 3,

    >>> import Data.Function (on)
    >>> groupBy (compare `on` (`rem` 3)) [1..10]
    [[3,6,9],[1,4,7,10],[2,5,8]]
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

{-|
    Merge a list of lists. Works with infinite lists of infinite lists.

    __Preconditions:__ Each list must be sorted, and the list of lists must be sorted by first element.

    >>> mergeMany [[1, 4, 9, 16], [1, 1, 2, 3], [2, 3, 5, 7]]
    [1, 1, 1, 2, 2, 3, 3, 4, 5, 7, 9, 16]
-}
mergeMany :: (Ord a) => [[a]] -> [a]
mergeMany xss = 
  let xss' = filter (not . List.null) xss
   in if List.null xss'
      then []
      else let n = Plane (filter (not . List.null) xss')
            in generate (PQueue.singleton (root n) n)

   where
     generate :: (Ord a) => MinPQueue a (Node a) -> [a]
     generate pq =
       case PQueue.minViewWithKey pq of
         Nothing -> []
         Just ((x, node), pq') -> x:(generate pq'')
           where pq'' = foldr (uncurry PQueue.insert) pq' (children node)


     null :: Node a -> Bool
     null (Plane xss) = List.null xss
     null (Line xs)   = List.null xs

     root :: Node a -> a
     root (Plane xss) = head (head xss)
     root (Line xs) = head xs

     children :: Node a -> [(a, Node a)]
     children node =
        map (\x -> (root x, x))
      $ filter (not . null)
      $ case node of
          (Line (x:xs))        -> [Line xs]
          (Plane ((x:xs):xss)) -> [Line xs, Plane xss]

data Node a = Plane [[a]] | Line [a]
  deriving (Show, Ord, Eq)
