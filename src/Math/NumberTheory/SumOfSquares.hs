-- |
-- Module      : Math.NumberTheory.SumOfSquares
-- Description : Express an natural number as the sum of squares
-- Copyright   : (c) Preetham Gujjula, 2024
-- License     : BSD-3-Clause
-- Maintainer  : libraries@mail.preetham.io
-- Stability   : experimental
--
-- Express an natural number as the sum of squares
module Math.NumberTheory.SumOfSquares
  ( -- * Expressing \(n\) as the sum of squares
    sumOfSquares,
    sumOfSquaresF,
    numSumOfSquares,
    numSumOfSquaresF,

    -- * Expressing \(n^2\) as the sum of squares
    sumOfSquaresOfSquare,
    sumOfSquaresOfSquareF,
    numSumOfSquaresOfSquare,
    numSumOfSquaresOfSquareF,
  )
where

import Data.List (partition, foldl')
import Math.NumberTheory.Prime.Factor (Factorization, factor)
import Math.NumberTheory.Roots (integerSquareRoot)

-- | @'sumOfSquares' n@ is all @(a, b)@ such that @a^2 + b^2 == n@. Note that
--   @a@ and @b@ are allowed to be negative.
sumOfSquares :: (Integral a) => a -> [(a, a)]
sumOfSquares = sumOfSquaresF . factor

-- | Like 'sumOfSquares', but takes the factorization of @n@.
sumOfSquaresF :: forall a. (Integral a) => Factorization a -> [(a, a)]
sumOfSquaresF fact =
  let ((_, e2), pe4k1s, pe4k3s) = partitionPrimes fact
      pe4k1Factors =
        flip map pe4k1s $ \(p, e) ->
          let (a, b) = sumOfSquares4k1 p
              half1 = take (e + 1) (iterate (mul (a, b)) (1, 0))
              half2 =
                reverse $
                  take (e + 1) (iterate (mul (b, a)) (1, 0))
           in zipWith mul half1 half2
      pe2Factor =
        if odd e2
          then let x = 2 ^ (e2 `quot` 2) in (x, x)
          else (2 ^ (e2 `quot` 2), 0)
      pe4k3Factor = (,0 :: a) $ product $ map (\(p, e) -> p ^ (e `quot` 2)) pe4k3s
   in if all (even . snd) pe4k3s
        then map (foldl' mul (1, 0)) $ map ([pe2Factor, pe4k3Factor] ++) $ sequence pe4k1Factors
        else []

mul :: (Integral a) => (a, a) -> (a, a) -> (a, a)
mul (a, b) (c, d) =
  let e = a * c - b * d
      f = a * d + b * c
   in if e > 0
        then (e, f)
        else (f, -e)

-- | @'numSumOfSquares' n@ is the number of @(a, b)@ such that @a^2 + b^2 == n@.
--   Note that @a@ and @b@ are allowed to be negative.
numSumOfSquares :: (Integral a) => a -> Int
numSumOfSquares = numSumOfSquaresF . factor

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

-- | @'sumOfSquaresOfSquare' n@ is all @(a, b)@ such that @a^2 + b^2 == n^2@.
--   Note that @a@ and @b@ are allowed to be negative.
sumOfSquaresOfSquare :: (Integral a) => a -> [(a, a)]
sumOfSquaresOfSquare = sumOfSquaresOfSquareF . factor

-- | Like 'sumSumOfSquaresOfSquare', but takes the factorization of @n@.
sumOfSquaresOfSquareF :: (Integral a) => Factorization a -> [(a, a)]
sumOfSquaresOfSquareF fact =
  let (p2, p4k1s, p4k3s) = partitionPrimes fact
   in undefined

sumOfSquaresOfSquareF' :: (Integral a) => Factorization a -> [(a, a)]
sumOfSquaresOfSquareF' fact =
  let (p4k3s, prest) = span (\(p, _) -> (p `rem` 4) == 3) fact
   in if any ((> 0) . snd) p4k3s
        then []
        else concatMap (\(p, e) -> replicate e (sumOfSquares4k1 p)) prest

-- | @'numSumOfSquaresOfSquare' n@ is the number of @(a, b)@ such that
--   @a^2 + b^2 == n^2@. Note that @a@ and @b@ are allowed to be negative.
numSumOfSquaresOfSquare :: (Integral a) => a -> [(a, a)]
numSumOfSquaresOfSquare = numSumOfSquaresOfSquareF . factor

-- | Like 'numSumOfSquaresOfSquare', but takes the factorization of @n@.
numSumOfSquaresOfSquareF :: (Integral a) => Factorization a -> [(a, a)]
numSumOfSquaresOfSquareF = undefined

sumOfSquaresNaive :: forall a. (Integral a) => a -> [(a, a)]
sumOfSquaresNaive p = go 0 sq
  where
    sq :: a
    sq = integerSquareRoot p

    go :: a -> a -> [(a, a)]
    go i j =
      if i == sq + 1 || j == 0
        then []
        else case compare (i * i + j * j) p of
          LT -> go (i + 1) j
          EQ -> (i, j) : go (i + 1) (j - 1)
          GT -> go i (j - 1)

sumOfSquares4k1 :: forall a. (Integral a) => a -> (a, a)
sumOfSquares4k1 p = go 0 sq
  where
    sq :: a
    sq = integerSquareRoot p

    go :: a -> a -> (a, a)
    go i j =
      if i == sq + 1 || j == -1
        then
          error $
            "sumOfSquares4k1: No solutions found. "
              ++ "Ensure that input is a prime of the form 4k+1."
        else case compare (i * i + j * j) p of
          LT -> go (i + 1) j
          EQ -> (i, j)
          GT -> go i (j - 1)
