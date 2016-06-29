{-|
    Module      : Hilbert.PriorityQueue.List
    Description : A simple implementation of priority queue
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    A very basic implementation of priority queue. This is useful only
    for debugging purposes.
-}

module Hilbert.PriorityQueue.List
  ( ListQueue
  ) where

import Hilbert.PriorityQueue.ADT as PQ
import Data.Function (on)
import Data.List (minimumBy, delete)

{-|
    A naive list-based priority queue implementation, only for debugging other
    priority queues.
-}
newtype ListQueue v p = ListQueue [(v, p)]
  deriving (Show)

getList (ListQueue list) = list
instance PriorityQueueADT ListQueue where
  empty = ListQueue []
  size = length . getList
  insert v p queue = ListQueue $ (v, p):(getList queue)
  peekMinP = minimumBy (on compare snd) . getList

  deleteMinP (ListQueue [x]) = (x, empty)
  deleteMinP (ListQueue ((v, p):rest)) =
          let ((v', p'), (ListQueue rest')) =
                      deleteMinP (ListQueue rest)
           in if p < p'
              then ((v, p), (ListQueue rest))
              else ((v', p'), (ListQueue ((v, p):rest')))

  deleteAllMinP queue = ((vals, minP), ListQueue newList)
    where (vals, newList) = delete (getList queue) minP
          minP = snd $ peekMinP queue

          -- Delete all values in the list with the given priority
          -- Return the deleted values and remaining list
          delete :: (Ord p) => [(v, p)] -> p -> ([v], [(v, p)])
          delete list p = foldl step ([], []) list
            where step (del, remain) (v, p') =
                    if p == p'
                    then (v:del, remain)
                    else (del, (v, p'):remain)
      
  fromList = ListQueue
