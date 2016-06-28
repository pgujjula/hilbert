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
  ( PriorityQueueADT(..)
  ) where

import Data.List (foldl')

{-|
    The abstract interface for priority queue. All the priority queues
    included in Hilbert implement this typeclass.
-}
class PriorityQueueADT q where
  -- Minimal complete definition: everything except
  --  peekMin and delMin

  -- | An empty priority queue.
  empty         ::            q v p

  -- | Returns whether the queue is empty.
  null          ::            q v p -> Bool

  -- | The size of the priority queue.
  size          ::            q v p -> Int

  -- | Insert a new value with the given priority into the queue. If the value
  -- | already exists, it is not overwritten.
  insert        :: (Ord p) => v -> p -> q v p -> q v p

  -- | Get the value with minimal priority from the PriorityQueue.
  peekMin       :: (Ord p) => q v p -> v

  -- | Get the lowest priority and value associated with that priority.
  peekMinP      :: (Ord p) => q v p -> (v, p)
  deleteMin     :: (Ord p) => q v p -> (v, q v p)
  deleteMinP    :: (Ord p) => q v p -> ((v, p), q v p)
  deleteAllMin  :: (Ord p) => q v p -> ([v], q v p)
  deleteAllMinP :: (Ord p) => q v p -> (([v], p), q v p)
  meld          :: (Ord p) => q v p -> q v p -> q v p
  fromList      :: (Ord p) => [(v, p)] -> q v p

  {-
     Default implementations
  -}
  null = (== 0) . size
  peekMin = fst . peekMinP
  peekMinP = fst . deleteMinP
  deleteMin    = (\((a, b), c) -> (a, c)) . deleteMinP
  deleteAllMin = (\((a, b), c) -> (a, c)) . deleteAllMinP

  -- Keep removing elements from one queue and add to the other
  meld q1 q2 = if Hilbert.PriorityQueue.ADT.null q1
               then q2
               else meld q1' q2'
        where ((v, p), q1') = deleteMinP q1
              q2' = insert v p q2

  -- Build up a queue by adding elements to an empty list
  fromList = foldl' insert' empty
    where insert' queue (v, p) = insert v p queue
