{-| Module      : Math.NumberTheory.Divisor
    Description : Divisors of integers.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Divisors of integers.
-}

module Math.NumberTheory.Divisor
    ( divides
    , divisors
    , divisorsF
    , divisorPairs
    , divisorPairsF
    ) where

import Control.Applicative            (liftA2)
import Data.List                      (foldl')

import Math.NumberTheory.Prime.Factor (Factorization, factor)

{-| a `divides` b is True iff there is an integer k such that a*k == b.

    >>> 5 `divides` 30
    True
    >>> 5 `divides` 31
    False
    >>> 0 `divides` 3
    False
    >>> 0 `divides` 0
    True
-}
divides :: (Integral a) => a -> a -> Bool
divides a b
    | a == 0    = b == 0
    | otherwise = b `rem` a == 0

{-| The positive divisors of an integer. Not necessarily in sorted order.

    >>> divisors 6
    [1, 3, 2, 6]
    >>> divisors (-4)
    [1, 2, 4]
    >>> divisors 0
    []
-}
divisors :: (Integral a) => a -> [a]
divisors = maybe [] divisorsF . factor . abs

{-| The positive divisors of an integer, from its factorization. Not necessarily
    in sorted order.

    >>> divisorsF [(2, 1), (3, 1)]
    [1, 3, 2, 6]
    >>> divisors []
    [1]
-}
divisorsF :: (Integral a) => Factorization a -> [a]
divisorsF = foldl' (liftA2 (*)) [1] . map pows
  where
    pows (p, e) = take (e + 1) $ iterate (*p) 1

{-| The positive divisors of an integer 'n', paired up such that every pair
    (a, b) satisfies a*b == abs n. Not in any particular order.

    >>> divisorPairs 6
    [(1, 6), (3, 2)]
    >>> divisorPairs (-9)
    [(1, 9), (3, 3)]
-}
divisorPairs :: (Integral a) => a -> [(a, a)]
divisorPairs = mkPairs . divisors

{-| The positive divisors of an integer 'n', from its prime factorization,
    paired up such that every pair (a, b) satisfies a*b == abs n. Not in any
    particular order.

    >>> divisorPairsF [(2, 1), (3, 1)]
    [(1, 6), (3, 2)]
    >>> divisorPairsF []
    [(1, 1)]
-}
divisorPairsF :: (Integral a) => Factorization a -> [(a, a)]
divisorPairsF = mkPairs . divisorsF

mkPairs :: [a] -> [(a, a)]
mkPairs xs = take ((length xs + 1) `div` 2) $ zip xs (reverse xs)
