{-# LANGUAGE ImportQualifiedPost #-}

-- | Module      : Math.NumberTheory.Prime.Factorization
--   Description : Functions related to primes.
--   Copyright   : (c) Preetham Gujjula, 2020
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Functions related to primes.
module Math.NumberTheory.Prime
  ( isPrime,
    primes,
    primesTo,
    primesFromTo,
    composites,
    compositesTo,
  )
where

import Data.Function ((&))
import Data.List.Ordered (minus)
import Data.Word (Word64)
import Math.NumberTheory.Prime.Sieve qualified as PS
import Math.NumberTheory.Roots (integerSquareRoot)

-- | An infinite list of the primes.
--
--   >>> take 8 primes
--   [2, 3, 5, 7, 11, 13, 17, 19]
primes :: [Int]
primes = fmap fromIntegral PS.primes

-- | A list of the primes up to (and including) a limit.
--
--   >>> primesTo 10
--   [2, 3, 5, 7]
primesTo :: Int -> [Int]
primesTo n | n < 0 = []
primesTo n = fmap fromIntegral . PS.primesTo . fromIntegral $ n

-- | A list of the primes between a start and end, inclusive.
--
--   >>> primesFromTo 10 20
--   [11, 13, 17, 19]
primesFromTo :: Int -> Int -> [Int]
primesFromTo a b | a <= 0 = primesTo b
primesFromTo a b =
  PS.primesFromTo (fromIntegral a - 1) (fromIntegral b)
    & map fromIntegral

-- | Whether a number is prime.
--
--   >>> filter isPrime [1..10]
--   [2, 3, 5, 7]
isPrime :: (Integral a) => a -> Bool
isPrime n | n < 2 = False
isPrime n = isPrime' . fromIntegral $ n

isPrime' :: Word64 -> Bool
isPrime' n = not $
  any (\p -> n `rem` p == 0) $
    takeWhile (<= integerSquareRoot n) PS.primes

-- | A lazy infinite list of composities.
--
--   >>> takeWhile (<= 20) composites
--   [4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20]
composites :: [Int]
composites = [2 ..] `minus` primes

-- | A lazy infinite list of composities, up to a limit. Will be faster than
--   'composites'.
--
--   >>> compositesTo 20
--   [4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20]
compositesTo :: Int -> [Int]
compositesTo n = [2 .. n] `minus` primesTo n
