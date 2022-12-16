{-| Module      : Math.NumberTheory.Prime.Factor
    Description : Prime factorizationa.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Prime factorization.
-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Math.NumberTheory.Prime.Factor
    ( Factorization
    , multiply
    , pow
    , simplify
    , factor
    , factorizations
    , factorizationsFrom
    , smallestFactor
    ) where

import Control.Monad (forM_)
import Data.Ratio ((%))
import Data.Tuple (swap)
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap
import           Data.List               (foldl', mapAccumL)
import           Data.Maybe              (fromMaybe)
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector

import           Data.List.Duplicate     (groupAdj)
import Data.Function ((&))
import Control.Monad.ST (runST)
import Data.Vector.Mutable qualified as MVector

import           Math.NumberTheory.Power (integralSqrt, square)
import           Math.NumberTheory.Prime (primes)

{-| Type to represent factorizations -}
type Factorization a = [(a, Int)]

{-| Multiply two factorizations together. -}
multiply :: (Integral a) => Factorization a -> Factorization a -> Factorization a
multiply xs [] = xs
multiply [] ys = ys
multiply ((px, ex):xs) ((py, ey):ys) =
    case compare px py of
        LT -> (px, ex)      : multiply xs ((py, ey):ys)
        EQ -> (px, ex + ey) : multiply xs ys
        GT -> (py, ey)      : multiply ((px, ex):xs) ys

{-| Raise the number represented by the given factorization to the
    specified power.
-}
pow :: Factorization a -> Int -> Factorization a
pow fs k = map (\(p, e) -> (p, e*k)) fs

{-| Multiply out a Factorization to get the number it represents. -}
simplify :: (Integral a) => Factorization a -> a
simplify = product . map (uncurry (^))

count :: (Ord a) => [a] -> [(a, Int)]
count = map (\xs -> (head xs, length xs)) . groupAdj

{-| Factor positive number.

    >>> factor 60
    Just [(2,2),(3,1),(5,1)]
    >>> factor 1
    Just []
    >>> factor 0
    Nothing
    >>> factor (-60)
    Nothing
-}
factor :: (Integral a) => a -> Maybe (Factorization a)
factor n
    | n <= 0    = Nothing
    | n == 1    = Just []
    | otherwise = Just
                $ count
                $ factorWith (map fromIntegral primes) (integralSqrt n) n

factorWith :: (Integral a) => [a] -> a -> a -> [a]
factorWith (p:ps) limit n
    | p > limit = [n]
    | r == 0      = p : factorWith (p:ps) (integralSqrt n') n'
    | otherwise   = factorWith ps limit n
  where
    (n', r) = n `quotRem` p
factorWith _ _ _ = error "ran out of primes, this is impossible"

-- smallestFactor !! i is the smallest prime factor of i + 2.
smallestFactor :: [Int]
smallestFactor = 2 : sieve (IntMap.singleton 4 [2]) [3..]
  where
    sieve mp (x:xs) =
        case IntMap.minViewWithKey mp of
            Nothing -> []
            Just ((n, ps), mp') ->
                case compare n x of
                    -- The first number in the map was too small. This should not happen
                    LT -> error "missing numbers from input stream"

                    -- Not a prime, and we have a list of witnesses ps to prove it.
                    -- Reinsert the primes at their next multiples.
                    EQ -> minimum ps : sieve (reinsert (n, ps) mp') xs

                    -- found a prime. yield it and insert it into the list at the first
                    -- multiple that matters: x^2
                    GT -> x : sieve (insert (x*x, x) mp) xs
    sieve _ [] = error "sieving empty list"

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

{-| An infinite list of the factorizations of [1..].

    >>> mapM_ print $ take 6 factorizations
    []
    [(2,1)]
    [(3,1)]
    [(2,2)]
    [(5,1)]
    [(2,1),(3,1)]
-}
factorizations :: [Factorization Int]
factorizations = factorizationsFrom 1

{-| An infinite list of the factorizations of [n..].

    >>> mapM_ print $ take 6 (factorizationsFrom 3)
    [(3,1)]
    [(2,2)]
    [(5,1)]
    [(2,1),(3,1)]
    [(7,1)]
    [(2,3)]
-}
factorizationsFrom :: Int -> [Factorization Int]
factorizationsFrom n = zipWith mkFactorization [n..] smallFactors
  where
    smallFactors :: [[Int]]
    smallFactors =
      sieveAllChunks n smallFactorSieve
      & concatMap Vector.toList

    mkFactorization :: Int -> [Int] -> Factorization Int
    mkFactorization k ps =
      case divideOutAll k ps of
        (1, fs) -> fs
        (p, fs) -> fs ++ [(p, 1)]
      where
        divideOutAll :: Int -> [Int] -> (Int, [(Int, Int)])
        divideOutAll = mapAccumL divideOut

        divideOut :: Int -> Int -> (Int, (Int, Int))
        divideOut t p = go 0 t
          where
            go :: Int -> Int -> (Int, (Int, Int))
            go !i t' =
              let (q, r) = t' `quotRem` p
               in if r == 0
                  then go (i+1) q
                  else (t', (p, i))

-- Generate the small prime factors (i.e. <=sqrt(n)) of the positive integers
-- in a streaming fashion.
--
-- The idea is that we split the positive integers into intervals of the form
-- [n .. n + O(sqrt(n))], and use the Sieve of Erastosthenes to factor the
-- numbers in each interval.
--
-- For a given interval, we only need to mark the multiples of primes up to
-- O(sqrt(n)). Since the interval width is O(sqrt(n)), each prime will have
-- at least O(1) multiple in the interval.
--
-- If the interval width is smaller than O(sqrt(n)), then many primes will have
-- less than O(1) expected multiples in the interval, and the efficiency of the
-- sieve drops. As an extreme case, if the interval width was 1, then the sieve
-- behavior degrades to trying every prime on every positive integer: this is
-- trial division!
--
-- We want to keep the interval width as small as possible to avoid excessive
-- memory use, so we don't want an interval width larger than O(sqrt(n)).

-- Each chunk represents an interval [n..m] as (sqrt m, n, m)
type Chunk = (Int, Int, Int)

-- Given a chunk [ni..mi] and all primes <= sqrt(mi), generate a Vector
-- containing the small prime factors of each integer in [ni..mi].
smallFactorSieve :: [Int] -> Chunk -> Vector [Int]
smallFactorSieve ps (_, n, m) = runST $ do
  v <- MVector.replicate (m - n + 1) []
  forM_ ps $ \p -> do
    let lower = p*ceiling (n % p)
        upper = p*(m `quot` p)
     in forM_ [lower, lower+p..upper] $ \i ->
          MVector.modify v (p:) (i-n)
  Vector.unsafeFreeze v

-- Split the positive integers into chunks [start..m0], [n1..m1], ..
mkChunks :: Int -> [Chunk]
mkChunks start = map (\a -> (a+1, square a + 1, square (a+1))) [a0..]
       & truncateHead
  where
    a0 = integralSqrt (start-1)
    truncateHead ((sqrtm, _, m):xs) = (sqrtm, start, m) : xs

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
