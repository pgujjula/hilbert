{-# LANGUAGE ScopedTypeVariables #-}
module Hilbert.Prime.List (primes) where

import Debug.Trace (trace)
import Hilbert.PriorityQueue
  (PriorityQueueADT
  , BinomialQueue
  , MapQueue
  , deleteMinWithPriority
  , fromList
  , meld
  , insert
  , deleteMin
  , empty)

import qualified Hilbert.PriorityQueue as PQ (null)

primes :: (Integral a) => [a]
primes = sieve [2..]

emptyQueue :: (Integral a) => BinomialQueue a a
emptyQueue = empty

sieve :: (Integral a) => [a] -> [a]
sieve = sieve' emptyQueue

sieve' :: (Integral a, PriorityQueueADT q) => q a a -> [a] -> [a]
sieve' queue (x:xs) = 
  let (list, newQueue) = deleteWhile (\v p -> p == x) queue
      updatedList = map (\(v, p) -> (v, p + v)) list
      updatedQueue = meld (fromList updatedList) newQueue
  in case list of
      [] -> x:(sieve' (insert x (2*x) updatedQueue) xs)
      otherwise -> sieve' updatedQueue xs

deleteWhile :: forall q v p. (PriorityQueueADT q, Ord p)
                          => (v -> p -> Bool)
                          -> q v p
                          -> ([(v, p)], q v p)
deleteWhile _ queue
  | PQ.null queue = ([], queue)
deleteWhile func queue = 
  let (potentialDelete, newQueue) = deleteMinWithPriority queue
      (val, pty) = potentialDelete
   in if func val pty
      then let (restList, finalQueue) = deleteWhile func newQueue
            in ((val, pty):restList, finalQueue)
      else ([], queue)

list :: [Int]
list = [1..10]

pairs = zip list (reverse list)

queue :: MapQueue Int Int
queue = fromList pairs

func v p = p <= 4
