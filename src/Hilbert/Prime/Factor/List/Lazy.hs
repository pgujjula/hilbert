{-|
    Module      : Hilbert.Prime.Factor.List.Lazy
    Description : Factor the positive integers lazily
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Factor the positive integers lazily
-}
module Hilbert.Prime.Factor.List.Lazy
  ( factorTo
  , factorToInf
  ) where

import Hilbert.PriorityQueue
import Hilbert.Square (integralSqrt)
import Data.List (foldl')

{-| 
    Factor the first @n@ positive integers lazily. Lower memory usage and faster
    than @take n factorizations@.
-}
factorTo :: Int -> [[(Int, Int)]]
factorTo n = zipWith clean [1..] $ [[], [(2, 1)]] ++ (stepAllLazy limit n (insert (2, tail (powers 2)) 4 em) 3)
  where em = empty :: MapQueue (Int, [Int]) Int
        limit = integralSqrt n

insertPair :: (PriorityQueueADT q, Ord p) => q v p -> (v, p) -> q v p
insertPair queue (v, p) = insert v p queue

-- Convert factorization to an integer
multiplyOut :: [(Int, Int)] -> Int
multiplyOut = product . (map (\(p, e) -> p^e))

clean :: Int -> [(Int, Int)] -> [(Int, Int)]
clean n fs = if leftover > 1
             then fs ++ [(leftover, 1)]
             else fs
    where leftover = n `div` (multiplyOut fs)

stepAllLazy :: (PriorityQueueADT q)
        => Int
        -> Int
        -> q (Int, [Int]) Int
        -> Int
        -> [[(Int, Int)]]
stepAllLazy limit maxN queue n | maxN < n = []
stepAllLazy limit maxN queue n = f:fs
  where (f, queue') = stepLazy limit queue n
        fs = stepAllLazy limit maxN queue' (n + 1)

stepLazy :: (PriorityQueueADT q)
     => Int
     -> q (Int, [Int]) Int
     -> Int
     -> ([(Int, Int)], q (Int, [Int]) Int)
stepLazy limit queue n | n /= minP  = ([(n, 1)], newQueue)
  where minP = snd $ peekMinP queue
        newQueue = if n <= limit
                   then insert (n, tail (powers n)) (2*n) queue
                   else queue

stepLazy limit queue n = (factorization, newQueue)
  where minP = snd $ peekMinP queue
        factorization :: [(Int, Int)]
        factorization = map (\(p, powerList) -> (p, head powerList)) primePowerList
        primePowerList :: [(Int, [Int])]
        (primePowerList, queue') = deleteAllMin queue
        reinsert :: [((Int, [Int]), Int)]
        reinsert = map (\(p, powerList) -> ((p, tail powerList), n + p)) primePowerList
        newQueue = foldl' insertPair queue' reinsert

powers :: Int -> [Int]
powers n = list
  where list = altLeft (n - 1) (repeat 1) (map (+1) list)

        altLeft :: Int -> [a] -> [a] -> [a]
        altLeft n xs ys = prev ++ (altRight n rest ys)
          where (prev, rest) = splitAt n xs

        altRight :: Int -> [a] -> [a] -> [a]
        altRight n xs (y:ys) = y:(altLeft n xs ys)

{-|
    Get a lazy infinite list of factorizations of the poistive integers.
    Equivalent to @map factor [1..]@

    >>> take 5 factorizations
    [[], [(2, 1)], [(3, 1)], [(2, 2)], [(5, 1)]]
-}
factorToInf :: [[(Int, Int)]]
factorToInf = [[], [(2, 1)]] ++ (stepAll (insert (2, tail (powers 2)) 4 em) 3)
  where em = empty :: MapQueue (Int, [Int]) Int

stepAll :: (PriorityQueueADT q)
        => q (Int, [Int]) Int
        -> Int
        -> [[(Int, Int)]]
stepAll queue n = f:fs
  where (f, queue') = step queue n
        fs = stepAll queue' (n + 1)
{-
   step queue computes the prime factorization of the minimum priority element 
   in the queue, reinserts any removed prime powers, and returns the 
   factorization and the new queue as a tuple.
-}
step :: (PriorityQueueADT q)
     => q (Int, [Int]) Int
     -> Int
     -> ([(Int, Int)], q (Int, [Int]) Int)
step queue n 
  | n /= minP  = ([(n, 1)], insert (n, tail (powers n)) (2*n) queue)
  | otherwise  = (factorization, newQueue)
  where minP = snd $ peekMinP queue
        factorization :: [(Int, Int)]
        factorization = map (\(p, powerList) -> (p, head powerList)) primePowerList
        primePowerList :: [(Int, [Int])]
        (primePowerList, queue') = deleteAllMin queue
        reinsert :: [((Int, [Int]), Int)]
        reinsert = map (\(p, powerList) -> ((p, tail powerList), n + p)) primePowerList
        newQueue = foldl' insertPair queue' reinsert
