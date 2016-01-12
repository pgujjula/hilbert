module Hilbert.Prime.List (primes) where

import Hilbert.PriorityQueue as PQ

primes :: [Int]
primes = primesFrom [2..] e
  where primesFrom :: [Int] -> PriorityQueue Int Int -> [Int]
        primesFrom xs mq = case step xs mq of
                             (Just p, rest, mq') -> p:(primesFrom rest mq')
                             (Nothing, rest, mq') -> primesFrom rest mq'

        takeEvery :: Int -> [a] -> [a]
        takeEvery n (x:xs) = x:(takeEvery n (drop (n - 1) xs))

        e :: PriorityQueue Int Int
        e = empty

        step :: [Int] -> PriorityQueue Int Int -> (Maybe Int, [Int], PriorityQueue Int Int)
        step (x:xs) mq
          | (PQ.null mq) || (Prelude.null facs) = (Just x, xs, insert x (x*x) mq)
          | otherwise = (Nothing, xs, insertAll facs' mq')
            where (facs, mq') = deleteWhile (\(v, p) -> p == x) mq
                  facs' = map (\(v, p) -> (v, v + p)) facs

        deleteWhile :: (Ord p) => ((v, p) -> Bool) -> (PriorityQueue v p) -> 
                       ([(v, p)], PriorityQueue v p)
        deleteWhile func mq
          | PQ.null mq = ([], mq)
          | func pair  = (pair:pairs, finalQueue)
          | otherwise = ([], mq)
              where (pair, mq') = deleteMinWithPriority mq
                    (pairs, finalQueue) = deleteWhile func mq'

        insertAll :: (Ord p) => [(v, p)] -> PriorityQueue v p -> PriorityQueue v p
        insertAll [] mq = mq
        insertAll ((v, p):xs) mq = insertAll xs (insert v p mq)


