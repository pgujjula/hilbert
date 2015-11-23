module Hilbert.PriorityQueue where

import Data.List (foldl')

class PriorityQueue q where
  -- Minimal complete definition
  --   insert
  --   peekMinWithPriority
  --   deleteMinWithPriority
  --   empty
  --   size
  -- Highly recommended to implement 'meld' and 'null' as well
  insert                :: (Ord p) => e -> p -> q e p -> q e p
  peekMin               :: (Ord p) => q e p -> e
  peekMinWithPriority   :: (Ord p) => q e p -> (e, p)
  deleteMin             :: (Ord p) => q e p -> (e, q e p)
  deleteMinWithPriority :: (Ord p) => q e p -> (e, p, q e p)
  meld                  :: (Ord p) => q e p -> q e p -> q e p
  fromList              :: (Ord p) => [(e, p)] -> q e p
  empty                 :: (Ord p) => q e p
  null                  ::            q e p -> Bool
  size                  ::            q e p -> Int

  -- Default implementations
  peekMin = fst . peekMinWithPriority

  deleteMin = (\(a, b, c) -> (a, c)) . deleteMinWithPriority

  meld queue1 queue2 =
    if Hilbert.PriorityQueue.null queue1
    then queue2
    else let (element, priority, queue1') = deleteMinWithPriority queue1
             queue2' = insert element priority queue2
          in meld queue1' queue2'

  fromList = foldl' (\queue (element, priority)
                        -> insert element priority queue)
                    Hilbert.PriorityQueue.empty

  null = (== 0) . size
