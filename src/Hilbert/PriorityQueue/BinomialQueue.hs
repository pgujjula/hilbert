module Hilbert.PriorityQueue.BinomialQueue
  ( BinomialQueue
  , empty) where

import Hilbert.PriorityQueue.BinomialQueue.BinomialTree (BinomialTree)

newtype BinomialQueue v p = BinomialQueue [Maybe (BinomialTree v p)]
  deriving (Show)

empty :: BinomialQueue v p
empty = undefined

null :: BinomialQueue v p -> Bool
null = undefined

size :: BinomialQueue v p -> Int
size = undefined

insert :: (Ord p) => v -> p -> BinomialQueue v p -> BinomialQueue v p
insert = undefined

peekMin :: BinomialQueue v p -> v
peekMin = undefined

peekMinWithPriority :: BinomialQueue v p -> (v, p)
peekMinWithPriority = undefined

deleteMin :: BinomialQueue v p -> (v, BinomialQueue v p)
deleteMin = undefined

deleteMinWithPriority :: BinomialQueue v p -> ((v, p), BinomialQueue v p)
deleteMinWithPriority = undefined

meld :: (Ord p) => BinomialQueue v p -> BinomialQueue v p -> BinomialQueue v p
meld = undefined

fromList :: (Ord p) => [(v, p)] -> BinomialQueue v p
fromList = undefined 
