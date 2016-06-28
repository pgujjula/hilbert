{-|
    Module      : Hilbert.PriorityQueue.Naive
    Description : A simple implementation of priority queue
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    A very basic implementation of priority queue. This is useful only
    for debugging purposes.
-}

module Hilbert.PriorityQueue.Naive
  ( NaiveQueue
  ) where

import Hilbert.PriorityQueue.ADT as PQ
import Data.Function (on)
import Data.List (minimumBy, delete)

{-|
    A list-based priority queue implementation, only for debugging other
    priority queues.
-}
newtype NaiveQueue v p = NaiveQueue [(v, p)]
  deriving (Show)

getList (NaiveQueue list) = list
instance PriorityQueueADT NaiveQueue where
  empty = NaiveQueue []
  size = length . getList
  insert v p queue = NaiveQueue $ (v, p):(getList queue)
  peekMinP = minimumBy (on compare snd) . getList

  deleteMinP (NaiveQueue [x]) = (x, empty)
  deleteMinP (NaiveQueue ((v, p):rest)) =
          let ((v', p'), (NaiveQueue rest')) =
                      deleteMinP (NaiveQueue rest)
           in if p < p'
              then ((v, p), (NaiveQueue rest))
              else ((v', p'), (NaiveQueue ((v, p):rest')))

  deleteAllMinP queue = ((vals, minP), NaiveQueue newList)
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
      
  fromList = NaiveQueue
