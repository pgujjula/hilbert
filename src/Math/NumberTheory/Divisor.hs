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

    , numDivisors
    , numDivisorsF

    , sumDivisors
    , sumDivisorsF

    , relativelyPrime

    , totient
    , totientF
    ) where

import Control.Applicative            (liftA2)
import Data.List                      (foldl')
import Data.Maybe                     (fromJust)

import Math.NumberTheory.Prime        (unPrime)
import Math.NumberTheory.Prime.Factor (Factorization, factor)

{-| @a `divides` b@ is True iff there is an integer @k@ such that @a*k == b@.

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

    >>> prime = unsafeMarkPrime
    >>> divisorsF [(prime 2, 1), (prime 3, 1)]
    [1, 3, 2, 6]
    >>> divisors []
    [1]
-}
divisorsF :: (Integral a) => Factorization a -> [a]
divisorsF = foldl' (liftA2 (*)) [1] . map pows
  where
    pows (p, e) = take (e + 1) $ iterate (* unPrime p) 1

{-| The positive divisors of an integer @n@, paired up such that every pair
    @(a, b)@ satisfies @a*b == abs n@. Not in any particular order.

    >>> divisorPairs 6
    [(1, 6), (3, 2)]
    >>> divisorPairs (-9)
    [(1, 9), (3, 3)]
-}
divisorPairs :: (Integral a) => a -> [(a, a)]
divisorPairs = mkPairs . divisors

{-| The positive divisors of an integer @n@, from its prime factorization,
    paired up such that every pair @(a, b)@ satisfies @a*b == abs n@. Not in any
    particular order.

    >>> prime = unsafeMarkPrime
    >>> divisorPairsF [(prime 2, 1), (prime 3, 1)]
    [(1, 6), (3, 2)]
    >>> divisorPairsF []
    [(1, 1)]
-}
divisorPairsF :: (Integral a) => Factorization a -> [(a, a)]
divisorPairsF = mkPairs . divisorsF

mkPairs :: [a] -> [(a, a)]
mkPairs xs = take ((length xs + 1) `div` 2) $ zip xs (reverse xs)

{-| The number of positive divisors of @n@.

    >>> numDivisors 12
    6
    >>> numDivisors 1
    1
    >>> numDivisors 0
    0
-}
numDivisors :: (Integral a) => a -> a
numDivisors = maybe 0 numDivisorsF . factor . abs

{-| The number of positive divisors of @n@, from its factorization.

    >>> prime = unsafeMarkPrime
    >>> numDivisorsF [(prime 2, 2), (prime 3, 1)]
    6
    >>> numDivisorsF []
    1
-}
numDivisorsF :: (Integral a) => Factorization a -> a
numDivisorsF = product . map (\(_, e) -> fromIntegral e + 1)

{-| The sum of the positive divisors of @n@.

    >>> sumDivisors 12
    28
    >>> sumDivisors 1
    1
    >>> sumDivisors 0
    0
-}
sumDivisors :: (Integral a) => a -> a
sumDivisors = maybe 0 sumDivisorsF . factor . abs

{-| The sum of the positive divisors of @n@, from its factorization.

    >>> prime = unsafeMarkPrime
    >>> sumDivisorsF [(prime 2, 2), (prime 3, 1)]
    28
    >>> sumDivisorsF []
    1
-}
sumDivisorsF :: (Integral a) => Factorization a -> a
sumDivisorsF = product . map f
  where
    f (p, e) = (p'^(e + 1) - 1) `div` (p' - 1)
      where
        p' = unPrime p

{-| @relativelyPrime m n@ is @True@ if @m@ and @n@ have no common positive
    divisors other than 1.

    >>> relativelyPrime 3 5
    True
    >>> relativelyPrime 4 6
    False
    >>> relativelyPrime 0 1
    True
    >>> relativelyPrime 0 0
    False
-}
relativelyPrime :: (Integral a) => a -> a -> Bool
relativelyPrime m n = gcd m n == 1

{-| @totient n@ is the number of integers in @[1..n]@ that are relatively prime
    to @n@. Also, @totient 0 == 1@ by convention.

    >>> totient 10
    4
    >>> totient (-4)
    0
-}
totient :: (Integral a) => a -> a
totient n
    | n < 0     = 0
    | n == 0    = 1
    | otherwise = totientF n (fromJust $ factor n)

{-| Like 'totient', but takes the factorization of @n@ to speed up the
    computation of the totient.

    >>> prime = unsafeMarkPrime
    >>> totient 10 [(prime 2, 1), (prime 5, 1)]
    4
-}
totientF :: (Integral a) => a -> Factorization a -> a
totientF = foldl' step
  where
    step n (p, _) = n * (p' - 1) `div` p'
      where
        p' = unPrime p
