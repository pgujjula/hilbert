module Hilbert.PriorityQueue.ADT
  ( PriorityQueueADT
  , empty
  , Hilbert.PriorityQueue.ADT.null -- So as not to conflict with Prelude.null
  , size
  , insert
  , peekMin
  , peekMinWithPriority
  , deleteMin
  , deleteMinWithPriority
  , meld
  , fromList) where

class PriorityQueueADT q where
  -- Minimal complete definition: everything except
  --  peekMin and deleteMin
  empty                 ::            q v p
  null                  ::            q v p -> Bool
  size                  ::            q v p -> Int
  insert                :: (Ord p) => v -> p -> q v p -> q v p
  peekMin               ::            q v p -> v
  peekMinWithPriority   ::            q v p -> (v, p)
  deleteMin             ::            q v p -> (v, q v p)
  deleteMinWithPriority ::            q v p -> ((v, p), q v p)
  meld                  :: (Ord p) => q v p -> q v p -> q v p
  fromList              :: (Ord p) => [(v, p)] -> q v p

  -- Default implementations
  peekMin = fst . peekMinWithPriority

  deleteMin = (\((a, b), c) -> (a, c)) . deleteMinWithPriority
