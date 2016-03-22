{-|
Module      : Hilbert.PriorityQueue.ADT
Description : The abstract interface for priority queues.
Copyright   : (c) Preetham Gujjula, 2016
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental

The abstract interface for priority queues.
-}

module Hilbert.PriorityQueue.ADT 
  ( PriorityQueueADT(..)) where

{-| The abstract interface for priority queue. All the priority queues
    included in Hilbert implement this typeclass.
-}
class PriorityQueueADT q where
  -- Minimal complete definition: everything except
  --  peekMin and deleteMin
  
  {-| An empty priority queue. -}
  empty                 ::            q v p

  {-| Returns whether the queue is empty. -}
  null                  ::            q v p -> Bool

  {-| The size of the priority queue. -}
  size                  ::            q v p -> Int

  {-| Insert a new value with the given priority into the queue. If the value 
      already exists, it is not overwritten. -}
  insert                :: (Ord p) => v -> p -> q v p -> q v p

  {-| Get the value with minimal priority from the PriorityQueue. -}
  peekMin               :: (Ord p) => q v p -> v

  {-| Get the lowest priority and value associated with that priority. -}
  peekMinWithPriority   :: (Ord p) => q v p -> (v, p)
  deleteMin             :: (Ord p) => q v p -> (v, q v p)
  deleteMinWithPriority :: (Ord p) => q v p -> ((v, p), q v p)
  meld                  :: (Ord p) => q v p -> q v p -> q v p
  fromList              :: (Ord p) => [(v, p)] -> q v p

  -- Default implementations
  peekMin = fst . peekMinWithPriority

  deleteMin = (\((a, b), c) -> (a, c)) . deleteMinWithPriority
