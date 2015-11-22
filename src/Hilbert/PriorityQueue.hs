module Hilbert.PriorityQueue
        (insert
        , deleteMin
        , empty
        , peekMin
        , PriorityQueue
        , Hilbert.PriorityQueue.null) where

import qualified Hilbert.BinomialQueue as BQ

insert :: (Ord p) => e -> p -> PriorityQueue e p -> PriorityQueue e p
insert elem priority (PriorityQueue binomialQueue) =
    PriorityQueue $ BQ.insert (Pair elem priority) binomialQueue

deleteMin :: (Ord p) => PriorityQueue e p -> (e, PriorityQueue e p)
deleteMin (PriorityQueue binomialQueue) = (elem, PriorityQueue newQueue)
  where elem = (\(Pair a b) -> a) pair
        (pair, newQueue) = BQ.deleteMin binomialQueue

empty = PriorityQueue (BQ.empty)

peekMin :: (Ord p) => PriorityQueue e p -> e
peekMin (PriorityQueue binomialQueue) = 
  (\(Pair a b) -> a) $ BQ.peekMin binomialQueue

null :: PriorityQueue e p -> Bool
null (PriorityQueue binomialQueue) = Prelude.null binomialQueue

data Pair e p = Pair e p

newtype PriorityQueue e p = PriorityQueue (BQ.BinomialQueue (Pair e p))

instance (Eq p) => Eq (Pair e p) where
  (Pair e1 p1) == (Pair e2 p2) = p1 == p2

instance (Ord p) => Ord (Pair e p) where
  compare (Pair e1 p1) (Pair e2 p2) = compare p1 p2
