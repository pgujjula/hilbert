-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Math.NumberTheory.Prime.SegmentedSieve
  ( sieveAllChunks,
    Chunk,
    smallestMultipleGE,
    largestMultipleLE,
  )
where

import Data.Function ((&))
import Data.List (mapAccumL)
import Data.Tuple (swap)
import Math.NumberTheory.Power (square)
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.Prime (primes)

type Chunk = (Int, Int, Int)

-- Split the positive integers into chunks [start..m0], [n1..m1], ..
mkChunks :: Int -> [Chunk]
mkChunks start =
  map (\a -> (a + 1, square a + 1, square (a + 1))) [a0 ..]
    & truncateHead
  where
    a0 = integerSquareRoot (start - 1)
    truncateHead ((sqrtm, _, m) : xs) = (sqrtm, start, m) : xs

-- For all i, concat (take i primeGroups) are all the primes that are
-- <= sqrt mi
mkPrimeGroups :: [Chunk] -> [[Int]]
mkPrimeGroups chunks = snd (mapAccumL f primes chunks)
  where
    f :: [Int] -> (Int, Int, Int) -> ([Int], [Int])
    f ps (sqrtm, _, _) = swap (span (<= sqrtm) ps)

-- Given a sieve that operates on a single chunk, sieve all the chunks in order
sieveAllChunks :: forall a. Int -> ([Int] -> Chunk -> a) -> [a]
sieveAllChunks start sieve = snd $ mapAccumL go [] (zip primeGroups chunks)
  where
    primeGroups :: [[Int]]
    primeGroups = mkPrimeGroups chunks

    chunks :: [Chunk]
    chunks = mkChunks start

    go :: [Int] -> ([Int], Chunk) -> ([Int], a)
    go oldPrimes (freshPrimes, chunk) =
      let newPrimes = reverse freshPrimes ++ oldPrimes
       in (newPrimes, sieve newPrimes chunk)

smallestMultipleGE :: Integral a => a -> a -> a
smallestMultipleGE p n = p * (((n - 1) `quot` p) + 1)

largestMultipleLE :: Integral a => a -> a -> a
largestMultipleLE p n = p * (n `quot` p)
