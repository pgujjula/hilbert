-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
-- |
-- Module      : Math.NumberTheory.SumOfSquares
-- Description : Express a natural number as the sum of squares
-- Copyright   : (c) Preetham Gujjula, 2024
-- License     : BSD-3-Clause
-- Maintainer  : libraries@mail.preetham.io
-- Stability   : experimental
--
-- Express a natural number as the sum of squares
module Math.NumberTheory.SumOfSquares
  ( -- * Listing solutions
    sumOfSquares,
    sumOfSquaresF,
    sumOfSquaresUnique,
    sumOfSquaresUniqueF,

    -- * Counting solutions
    numSumOfSquares,
    numSumOfSquaresF,
    numSumOfSquaresLE,
    numSumOfOddSquaresLE,
  )
where

import Data.Bifunctor (bimap)
import Data.Int (Int64)
import Data.List (foldl', partition)
import Data.Tuple (swap)
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import Math.NumberTheory.Prime.Factor (Factorization, factor)
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.SumOfSquares.Internal (sumOfSquaresUniqueNaive)

-- | @'sumOfSquares' n@ is all @(a, b)@ such that @a^2 + b^2 == n@. Note that
--   @a@ and @b@ are allowed to be negative.
sumOfSquares :: (Integral a) => a -> [(a, a)]
sumOfSquares n =
  case compare n 0 of
    LT -> []
    EQ -> [(0, 0)]
    GT -> sumOfSquaresF (factor n)

-- | Like 'sumOfSquares', but takes the factorization of @n@.
sumOfSquaresF :: forall a. (Integral a) => Factorization a -> [(a, a)]
sumOfSquaresF = concatMap rot . sumOfSquaresProperF

rot :: (Integral a) => (a, a) -> [(a, a)]
rot (x, y) = [(x, y), (-y, x), (-x, -y), (y, -x)]

-- | @'sumOfSquaresUnique' n@ is all @(a, b)@ such that @0 <= a <= b@ with
--   @a^2 + b^2 == n@. Note that @a@ and @b@ are allowed to be negative.
sumOfSquaresUnique :: (Integral a) => a -> [(a, a)]
sumOfSquaresUnique n =
  case compare n 0 of
    LT -> []
    EQ -> [(0, 0)]
    GT -> sumOfSquaresUniqueF (factor n)

-- | Like 'sumOfSquaresUnique', but takes the factorization of @n@.
sumOfSquaresUniqueF :: (Integral a) => Factorization a -> [(a, a)]
sumOfSquaresUniqueF = map swap . filter (\(a, b) -> 0 <= b && a >= b) . sumOfSquaresProperF

sumOfSquaresProperF :: forall a. (Integral a) => Factorization a -> [(a, a)]
sumOfSquaresProperF fact =
  let ((_, e2), pe4k1s, pe4k3s) = partitionPrimes fact
      pe4k1Factors =
        flip map pe4k1s $ \(p, e) ->
          let (a, b) = sumOfSquares4k1Memo p
              half1 = take (e + 1) (iterate (mul (a, b)) (1, 0))
              half2 =
                reverse $
                  take (e + 1) (iterate (mul (b, a)) (1, 0))
           in zipWith mul half1 half2
      pe2Factor =
        if odd e2
          then let x = 2 ^ (e2 `quot` 2) in (x, x)
          else (2 ^ (e2 `quot` 2), 0)
      pe4k3Factor =
        (,0 :: a) $ product $ map (\(p, e) -> p ^ (e `quot` 2)) pe4k3s
   in if all (even . snd) pe4k3s
        then
          map (mul (mul pe2Factor pe4k3Factor)) $
            foldl' combine [(1, 0)] pe4k1Factors
        else []

combine :: (Integral a) => [(a, a)] -> [(a, a)] -> [(a, a)]
combine = liftA2 mul

-- | @'numSumOfSquares' n@ is the number of @(a, b)@ such that @a^2 + b^2 == n@.
--   Note that @a@ and @b@ are allowed to be negative.
numSumOfSquares :: (Integral a) => a -> Int
numSumOfSquares n =
  case compare n 0 of
    LT -> 0
    EQ -> 1
    GT -> numSumOfSquaresF . factor $ n

partitionPrimes ::
  (Integral a) =>
  Factorization a ->
  ((a, Int), Factorization a, Factorization a)
partitionPrimes fact =
  let (pe2, peOdd) =
        case fact of
          (2, e) : fact' -> ((2, e), fact')
          _ -> ((2, 0), fact)
      (pe4k1s, pe4k3s) = partition (\(p, _) -> p `rem` 4 == 1) peOdd
   in (pe2, pe4k1s, pe4k3s)

-- | Like 'numSumOfSquares', but takes the factorization of @n@.
numSumOfSquaresF :: (Integral a) => Factorization a -> Int
numSumOfSquaresF fact =
  let (_, p4k1s, p4k3s) = partitionPrimes fact
   in if all (even . snd) p4k3s
        then 4 * product (map (\(_, e) -> e + 1) p4k1s)
        else 0

memoLimit :: Int
memoLimit = 10 ^ (6 :: Int)

-- Given a prime p = 4k + 1, find the unique solution (a, b) with 0 < a < b of
-- a^2 + b^2 == p.
sumOfSquares4k1Memo :: forall a. (Integral a) => a -> (a, a)
sumOfSquares4k1Memo p =
  if p <= fromIntegral memoLimit
    then bimap fromIntegral fromIntegral (sumOfSquares4k1Vec ! fromIntegral p)
    else sumOfSquares4k1 p

sumOfSquares4k1Vec :: Vector (Int, Int)
sumOfSquares4k1Vec = Vector.generate memoLimit sumOfSquares4k1

sumOfSquares4k1 :: forall a. (Integral a) => a -> (a, a)
sumOfSquares4k1 p =
  case sumOfSquaresUniqueNaive p of
    x : _ -> x
    [] ->
      error $
        "sumOfSquares4k1: No solutions found. "
          ++ "Ensure that input is a prime of the form 4k+1."

mul :: (Integral a) => (a, a) -> (a, a) -> (a, a)
mul (a, b) (c, d) =
  let e = a * c - b * d
      f = a * d + b * c
   in if e > 0
        then (e, f)
        else (f, -e)

-- | @'numSumOfSquaresLE' n@ is the number of nonnegative integers @(a, b)@ such
-- that @a^2 + b^2 == n@.
numSumOfSquaresLE :: Int -> Int
numSumOfSquaresLE n =
  let sq = integerSquareRoot (max n 0)
      sq2 = integerSquareRoot (max (n `quot` 2) 0)
   in fromIntegral $
        numSumOfSquaresLE_
          (fromIntegral n)
          (fromIntegral sq)
          (fromIntegral sq2)

foreign import ccall unsafe "num_sum_of_squares_le"
  numSumOfSquaresLE_ :: Int64 -> Int64 -> Int64 -> Int64

-- | @'numSumOfOddSquaresLE' n@ is the number of nonnegative odd integers
-- @(a, b)@ such that @a^2 + b^2 == n@.
numSumOfOddSquaresLE :: Int -> Int
numSumOfOddSquaresLE n =
  let sq = integerSquareRoot (max n 0)
      sq2 = integerSquareRoot (max (n `quot` 2) 0)
   in fromIntegral $
        numSumOfOddSquaresLE_
          (fromIntegral n)
          (fromIntegral sq)
          (fromIntegral sq2)

foreign import ccall unsafe "num_sum_of_odd_squares_le"
  numSumOfOddSquaresLE_ :: Int64 -> Int64 -> Int64 -> Int64
