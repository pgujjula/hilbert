{-# LANGUAGE ScopedTypeVariables #-}
{-| Module      : Math.NumberTheory.Digit
    Description : Functions related to the binomial coefficient.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

Functions related to the binomial coefficient.
-}

module Math.Combinatorics.Binomial
    ( factorial
    , choose
    , binomialCoeffs
    , pascalDiagonal
    , permute
    ) where

import Numeric.Natural (Natural)
import Prelude hiding (quot)
import Data.List (foldl', scanl')
import Data.Euclidean (Euclidean, quot)
import Data.Semiring (Semiring, product', fromNatural, one, zero, times, plus)

{-| The factorial of a number. Undefined behavior for negative inputs.

    >>> factorial 5
    120
-}
factorial :: (Integral a, Semiring b) => a -> b
factorial n | n <= 0 = one
factorial n = product' $ map (fromNatural . fromIntegral) [1..n]

{-| The binomial coefficient. @choose n k@ is defined as 0 for any @n < 0@ or
    @k > n@ or @k < 0@.

    >>> 5 `choose` 2
    10
-}
choose :: (Integral a, Euclidean b) => a -> a -> b
choose n k
    | n < 0     = zero
    | k > n     = zero
    | k < 0     = zero
    | 2*k > n   = choose' n (n - k)
    | otherwise = choose' n k

-- no preconditions are checked
choose' :: forall a b. (Integral a, Euclidean b) => a -> a -> b
choose' n k =
  foldl' (\i (p, q) -> (i `times` p) `quot` q) one
  $ zip nums denoms
    where
      nums :: [b]
      nums = map (fromNatural . fromIntegral) [n, n - 1..n-k+1] 

      denoms :: [b]
      denoms = map (fromNatural . fromIntegral) [1..k]

{-| Given n, yields the binomial coefficients of exponent n. More efficent than
    mapping 'choose'. Undefined behavior if @n < 0@.
-}
binomialCoeffs :: forall a b. (Integral a, Euclidean b) => a -> [b]
binomialCoeffs n | n < 0 = error "binomialCoeffs: negative input"
binomialCoeffs n = scanl' step one [0..n - 1]
  where
    step :: b -> a -> b
    step x k = (x `times` aToB (n - k)) `quot` aToB (k + 1)

    aToB :: a -> b
    aToB = fromNatural . fromIntegral

-- | The @n@th diagonal of Pascal's triangle. So,
--
-- > pascalDiagonal n == map (\m -> (n+m) `choose` m) [0..]
--
-- but more efficient
--
-- n `choose` 0    = n! / n! 0!      = 1
-- n+1 `choose` 1  = (n+1)! / n! 1!  = (n+1) `quot` 1
-- n+2 `choose` 2  = (n+2)! / n! 2!  = (n+2)*(n+1) `quot` 1*2
--                                   = (n+3)*(n+2)*(n+1) `quot` 1*2*3
pascalDiagonal :: forall a b. (Integral a, Euclidean b) => a -> [b]
pascalDiagonal n = scanl' step one [1..]
  where
    n' :: b
    n' = fromNatural . fromIntegral $ n

    step :: b -> Natural -> b
    step term m =
      let m' = fromNatural m
       in ((n' `plus` m') `times` term) `quot` m'

{-| Number of permutations groups of size @k@, selected from a group of size
    @n@. @permute n k@ is defined as 0 for any @n < 0@ or @k > n@ or @k < 0@.

    >>> 5 `permute` 2
    20
-}
permute :: (Integral a, Semiring b) => a -> a -> b
permute n k
    | n < 0     = zero
    | k > n     = zero
    | k < 0     = zero
    | otherwise = product' $ map (fromNatural . fromIntegral) [n, n - 1..n-k+1]
