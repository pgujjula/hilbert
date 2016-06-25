{-|
    Module      : Hilbert.Prime.List
    Description : An infinite list of prime numbers.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    An infinite list of prime numbers. This module uses mathematics described in
    
        * Neill, Melissa O. "The Genuine Sieve of Eratosthenes." Journal of
          Functional Programming 19.1 (2009): 95-106. Cambridge University
          Press. Web.
-}
{-# LANGUAGE FlexibleContexts #-}
module Hilbert.Prime.List
  ( primes
  , primesLessThan
  ) where

import Hilbert.PriorityQueue as PQ
import Hilbert.Square (integralSqrt)
import Data.Array.ST
import Data.Array.IArray (assocs)
import Control.Monad.ST
import Control.Monad (forM_)

{-|
   A lazy, infinite list of primes. The algorithm used is described in
    
        * Neill, Melissa O. "The Genuine Sieve of Eratosthenes." Journal of
          Functional Programming 19.1 (2009): 95-106. Cambridge University
          Press. Web.

   >>> take 10 primes
   [2, 3, 5, 7, 11, 13, 17, 19, 23]
-}
primes :: [Int]
primes = sieve [2..] (empty :: PriorityQueue a a)

-- Filter out all the primes in 'x:xs' using 'queue'
sieve :: (Integral a, PriorityQueueADT q) => [a] -> q a a -> [a]
sieve (x:xs) queue = if prime
                     then x:rest
                     else rest
  where (prime, queue') = process x queue
        rest = sieve xs queue'

{-
   'queue' contains all discovered primes, with their next unprocessed multiple
   as their priority.
   
   'process n queue' uses 'queue' to check whether 'n' is prime. If it is prime,
   then 'n' is inserted into 'queue' with priority 'n^2'. Otherwise, each prime
   'p' that had priority 'n' will be reinserted into 'queue' will priority
   'n + p'.
   
   The primality of n and the new queue are returned as a tuple.
   'n'.
-}
process :: (Integral a, PriorityQueueADT q) => a -> q a a -> (Bool, q a a)
process n queue = (prime, newQueue)
  where prime = not (minPriorityEquals n queue)
        newQueue = if prime
                   then insert n (n^2) queue
                   else insertAll primePairs queueWithoutN
        primePairs = map (\p -> (p, n + p)) removedPrimes
        (removedPrimes, queueWithoutN) = deleteAllWithPriority n queue

insertAll :: (PriorityQueueADT q, Ord p) => [(v, p)] -> q v p -> q v p
insertAll [] queue = queue
insertAll ((v, p):rest) queue = insertAll rest (insert v p queue)

-- Get the minimum priority of a nonempty queue.
minPriority :: (PriorityQueueADT q, Ord p) => q v p -> p
minPriority = snd . peekMinWithPriority

-- True if 'queue' is nonempty and the minimum priority in 'queue' equals 'pty'
minPriorityEquals :: (PriorityQueueADT q, Ord p) => p -> q v p -> Bool
minPriorityEquals pty queue =
    if PQ.null queue
    then False
    else minPriority queue == pty

{-
   Delete all the values with minimum priority from 'queue'. Return the 
   deleted values, the minimum priority, and the new queue as a triple. 

   Precondition: 'queue' is not empty
-}
deleteAllMin :: (PriorityQueueADT q, Ord p) => q v p -> ([v], p, q v p)
deleteAllMin queue = (values, minPty, newQueue)
  where minPty = minPriority queue
        (values, newQueue) = deleteAllWithPriority minPty queue

{-
   If 'pty' is the minimum priority in 'queue', deletes all the values with
   priority 'pty'. Returns the deleted values and the new queue.
   If 'pty' is not the minimum priority, returns an empty list and 'queue'.

   Precondition: 'queue' is not empty

   Warning: The "With" in "deleteAllWithPriority" and "deleteMinWithPriority"
            mean different things.
-}
deleteAllWithPriority :: (PriorityQueueADT q, Ord p) => p -> q v p -> ([v], q v p)
deleteAllWithPriority pty queue
  | minPty /= pty = ([], queue)
  | otherwise   = (values, newQueue) 
      where ((minVal, minPty), queue') = deleteMinWithPriority queue
            (values', newQueue)  = deleteAllWithPriority pty queue'
            values = minVal:values'

{-|
    Get a list of all the primes below the given limit. Much faster than
    @takeWhile (<= n) primes@.

    >>> primesLessThan 15
    [2, 3, 5, 7, 11, 13]
-}
primesLessThan :: Int -> [Int]
primesLessThan n | n < 2 = []
primesLessThan n = map fst $ filter snd $ assocs $ runSTUArray $ do
  array <- newArray (1, n - 1) True :: ST s (STUArray s Int Bool)
  writeArray array 1 False
  sieveAll array 2 (integralSqrt (n - 1)) (n - 1)
  return array

{-
   Sieve all the primes between 'p' and 'limit' inclusive. The upper bound
   of the array is also passed in as 'n'.
-}
sieveAll :: (MArray a Bool m) => a Int Bool -> Int -> Int -> Int -> m ()
sieveAll array p limit n | p > limit = return ()
sieveAll array p limit n = do
  writeAllFalse array [2*p, 3*p..n]
  p <- findTrue array (p + 1)
  sieveAll array p limit n

-- Return the first true index starting from 'n'
findTrue :: (MArray a Bool m) => a Int Bool -> Int -> m Int
findTrue array n = do
  b <- readArray array n
  if b
  then return n
  else findTrue array (n + 1) 

-- Write False at all the given indices
writeAllFalse :: (MArray a Bool m) => a Int Bool -> [Int] -> m ()
writeAllFalse array xs = forM_ xs (\x -> writeArray array x False)
