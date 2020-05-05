{-| Module      : Math.NumberTheory.Prime
    Description : Functions related to primes.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Functions related to primes.
-}

module Math.NumberTheory.Prime
     ( isPrime
     , primes
     , primesTo

--     , Factorization
--     , factor
--     , factorizations
--     , factorizationsTo
     ) where

import qualified Data.IntMap as IntMap
import           Data.IntMap           (IntMap, (!))
import           Data.Maybe (fromMaybe)
import           Data.List  (foldl')
import           Data.Function ((&))

import           Math.NumberTheory.Power (integralSqrt)

{-| Whether a number is prime.

    >>> filter isPrime [1..10]
    [2, 3, 5, 7]
-}
isPrime :: (Integral a) => a -> Bool
isPrime n
  | n < 2     = False
  | otherwise = not $ any (\p -> n `rem` p == 0)
              $ takeWhile (<= integralSqrt n) $ map fromIntegral primes

{-| A lazy infinite list of primes.
    
    >>> take 10 primes
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
-}
primes :: [Int]
primes = 2 : sieve (IntMap.singleton 4 [2]) [3..]
  where
    sieve :: IntMap [Int] -> [Int] -> [Int]
    sieve mp (x:xs) =
        case compare n x of
          -- The first number in the map was too small. Reinsert its prime
          -- factors and try sieving the same list again.
          LT -> sieve (reinsert (n, ps) mp') (x:xs)

          -- Not a prime, and we have a list of witnesses ps to prove it.
          -- Reinsert the primes at their next multiples.
          EQ -> sieve (reinsert (n, ps) mp') xs

          -- found a prime. yield it and insert it into the list at the first
          -- multiple that matters: x^2
          GT -> x : sieve (insert (x^2, x) mp) xs
      where
        ((n, ps), mp') = IntMap.deleteFindMin mp

    -- Given a number and its prime divisors, generate a list of the next
    -- multiple for each prime, so we can reinsert these into the map.
    updates :: (Int, [Int]) -> [(Int, Int)]
    updates (n, ps) = map (\p -> (n + p, p)) ps

    -- Insert a (number, prime divisor) pair into a map
    insert :: (Int, Int) -> IntMap [Int] -> IntMap [Int]
    insert (n, p) mp = IntMap.alter (pushPrime p) n mp
      where
        pushPrime q xs = Just (q : fromMaybe [] xs)

    -- After popping a number and its prime divisors, reinsert each prime back
    -- into the map at its next multiple.
    reinsert :: (Int, [Int]) -> IntMap [Int] -> IntMap [Int]
    reinsert (d, ps) mp = foldl' (flip insert) mp (updates (d, ps))


{-| A lazy infinite list of primes, up to a limit. Will be faster than 'primes'.
    
    >>> primesTo 30
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
-}
-- TODO: Factor out commonalities between primes and primesTo
primesTo :: Int -> [Int]
primesTo m = 2 : sieve (IntMap.singleton 4 [2]) [3..]
  where
    sieve :: IntMap [Int] -> [Int] -> [Int]
    sieve mp (x:xs) =
        case IntMap.minViewWithKey mp of
            Nothing -> []
            Just ((n, ps), mp') ->
                case compare n x of
                  -- The first number in the map was too small. Reinsert its prime
                  -- factors and try sieving the same list again.
                  LT -> sieve (reinsert (n, ps) mp') (x:xs)

                  -- Not a prime, and we have a list of witnesses ps to prove it.
                  -- Reinsert the primes at their next multiples.
                  EQ -> sieve (reinsert (n, ps) mp') xs

                  -- found a prime. yield it and insert it into the list at the first
                  -- multiple that matters: x^2
                  GT -> x : sieve (insert (x^2, x) mp) xs
      where
        ((n, ps), mp') = IntMap.deleteFindMin mp

    -- Given a number and its prime divisors, generate a list of the next
    -- multiple for each prime, so we can reinsert these into the map.
    updates :: (Int, [Int]) -> [(Int, Int)]
    updates (n, ps) = map (\p -> (n + p, p)) ps

    -- Insert a (number, prime divisor) pair into a map
    insert :: (Int, Int) -> IntMap [Int] -> IntMap [Int]
    insert (n, p) mp
        | n > m     = mp
        | otherwise = IntMap.alter (pushPrime p) n mp
      where
        pushPrime q xs = Just (q : fromMaybe [] xs)

    -- After popping a number and its prime divisors, reinsert each prime back
    -- into the map at its next multiple.
    reinsert :: (Int, [Int]) -> IntMap [Int] -> IntMap [Int]
    reinsert (d, ps) mp = foldl' (flip insert) mp (updates (d, ps))


type Factorization a = [(a, Int)]

factor :: (Integral a) => a -> Factorization a
factor = undefined

factorizations :: [Factorization Int]
factorizations = undefined

factorizationsTo :: Int -> [Factorization Int]
factorizationsTo = undefined
