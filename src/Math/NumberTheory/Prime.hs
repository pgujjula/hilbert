{-| Module      : Math.NumberTheory.Prime.Factorization
    Description : Functions related to primes.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Functions related to primes.
-}

module Math.NumberTheory.Prime
    ( isPrime
    , primes
    , primesTo
    , composites
    , compositesTo
    , Prime
    , unPrime
    , unsafeMarkPrime
    ) where

import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap
import           Data.List               (foldl')
import           Data.Maybe              (fromMaybe)

import           Data.List.Ordered       (minus)

import           Math.NumberTheory.Power (integralSqrt)

newtype Prime a = Prime {unPrime :: a}
    deriving (Show, Eq, Ord)

instance Functor Prime where
    fmap f (Prime a) = Prime (f a)

unsafeMarkPrime :: a -> Prime a
unsafeMarkPrime = Prime

{-| Whether a number is prime.

    >>> filter isPrime [1..10]
    [2, 3, 5, 7]
-}
isPrime :: (Integral a) => a -> Bool
isPrime n
  | n < 2     = False
  | otherwise = not $ any (\p -> n `rem` p == 0)
              $ takeWhile (<= integralSqrt n)
              $ map (fromIntegral . unPrime) primes

{-| A lazy infinite list of primes.

    >>> take 10 $ map unPrime primes
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
-}
primes :: [Prime Int]
primes = map unsafeMarkPrime (2 : sieve (IntMap.singleton 4 [2]) [3..])
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
          GT -> x : sieve (insert (x*x, x) mp) xs
      where
        ((n, ps), mp') = IntMap.deleteFindMin mp
    sieve _ [] = error "sieving empty list, this should not be possible"

    -- Given a number and its prime divisors, generate a list of the next
    -- multiple for each prime, so we can reinsert these into the map.
    updates :: (Int, [Int]) -> [(Int, Int)]
    updates (n, ps) = map (\p -> (n + p, p)) ps

    -- Insert a (number, prime divisor) pair into a map
    insert :: (Int, Int) -> IntMap [Int] -> IntMap [Int]
    insert (n, p) = IntMap.alter (pushPrime p) n
      where
        pushPrime q xs = Just (q : fromMaybe [] xs)

    -- After popping a number and its prime divisors, reinsert each prime back
    -- into the map at its next multiple.
    reinsert :: (Int, [Int]) -> IntMap [Int] -> IntMap [Int]
    reinsert (d, ps) mp = foldl' (flip insert) mp (updates (d, ps))


{-| A lazy infinite list of primes, up to a limit. Will be faster than 'primes'.

    >>> map unPrime $ primesTo 30
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
-}
-- TODO: Factor out commonalities between primes and primesTo
primesTo :: Int -> [Prime Int]
primesTo m = map unsafeMarkPrime (2 : sieve (IntMap.singleton 4 [2]) [3..])
  where
    sieve :: IntMap [Int] -> [Int] -> [Int]
    sieve mp (x:xs) =
        case IntMap.minViewWithKey mp of
            Nothing -> [x..m]
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
                  GT -> x : sieve (insert (x*x, x) mp) xs
    sieve _ [] = error "sieving empty list, this should not be possible"

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

{-| A lazy infinite list of composities.

    >>> takeWhile (<= 20) composites
    [4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20]
-}
composites :: [Int]
composites = [2..] `minus` map unPrime primes

{-| A lazy infinite list of composities, up to a limit. Will be faster than
    'composites'.

    >>> compositesTo 20
    [4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20]
-}
compositesTo :: Int -> [Int]
compositesTo n = [2..n] `minus` map unPrime (primesTo n)
