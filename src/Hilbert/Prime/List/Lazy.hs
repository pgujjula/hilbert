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

module Hilbert.Prime.List.Lazy
  ( primes
  , primesTo
  ) where

import Hilbert.PriorityQueue as PQ

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
        (removedPrimes, queueWithoutN) = deleteAllMin queue

insertAll :: (PriorityQueueADT q, Ord p) => [(v, p)] -> q v p -> q v p
insertAll [] queue = queue
insertAll ((v, p):rest) queue = insertAll rest (insert v p queue)

-- Get the minimum priority of a nonempty queue.
minPriority :: (PriorityQueueADT q, Ord p) => q v p -> p
minPriority = snd . peekMinP

-- True if 'queue' is nonempty and the minimum priority in 'queue' equals 'pty'
minPriorityEquals :: (PriorityQueueADT q, Ord p) => p -> q v p -> Bool
minPriorityEquals pty queue =
    if PQ.null queue
    then False
    else minPriority queue == pty


primesTo :: Int -> [Int]
primesTo n = undefined
