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
    , permute
    ) where

import Data.List (foldl', genericTake)

{-| The factorial of a number. Undefined behavior for negative inputs.

    >>> factorial 5
    120
-}
factorial :: (Integral a) => a -> a
factorial n = foldl' (*) 1 [1..n]

{-| The binomial coefficient. @choose n k@ is defined as 0 for any @n < 0@ or
    @k > n@ or @k < 0@.

    >>> 5 `choose` 2
    10
-}
choose :: (Integral a) => a -> a -> a
choose n k
    | n < 0          = 0
    | k > n          = 0
    | k < 0          = 0
    | k > n `quot` 2 = choose' n (n - k)
    | otherwise      = choose' n k

-- no preconditions are checked
choose' :: (Integral a) => a -> a -> a
choose' n k = foldl' (\i (p, q) -> i * p `quot` q) 1
            $ genericTake k
            $ zip [n, n - 1..] [1..]

{-| Given n, yields the binomial coefficients of exponent n. More efficent than
    mapping 'choose'. Undefined behavior if @n < 0@.
-}
binomialCoeffs :: Integral a => a -> [a]
binomialCoeffs n | n < 0 = error "binomialCoeffs: negative input"
binomialCoeffs n = scanl step 1 [0..n - 1]
  where
    step x k = x * (n - k) `div` (k + 1)

{-| Number of permutations groups of size @k@, selected from a group of size
    @n@. @permute n k@ is defined as 0 for any @n < 0@ or @k > n@ or @k < 0@.

    >>> 5 `permute` 2
    20
-}
permute :: (Integral a) => a -> a -> a
permute n k
    | n < 0     = 0
    | k > n     = 0
    | k < 0     = 0
    | otherwise = foldl' (*) 1 $ genericTake k [n, n - 1..]
