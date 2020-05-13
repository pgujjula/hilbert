{-|
    Module      : Math.NumberTheory.Modular
    Description : Functions to perform modular arithmetic.
    Copyright   : (c) Preetham Gujjula, 2016 - 2020
    License     : GPL-3
    Maintainer  : pgujjula+hilbert@protonmail.com
    Stability   : experimental

    Functions to perform modular arithmetic.
-}

module Math.NumberTheory.Modular
    ( modPow
    , egcd
    , modInv
    ) where

import Math.NumberTheory.Power (square)

{-|
    @modPow a b m @ efficiently computes @mod (a^b) m@.

    __Preconditions:__

        * a >= 0
        * b >= 0
        * m > 0


    >>> modPow 3 4 7
    4
    >>> modPow 0 10 12
    0
    >>> modPow 13 0 23
    1
    >>> modPow 0 0 19
    1
-}
modPow :: (Integral a) => a -> a -> a -> a
modPow a b m = fromIntegral $ modPowLowLevel (toInteger a) (toInteger b) (toInteger m)

{-
   Compute modular power without worring about overflow.
-}
modPowLowLevel :: Integer -> Integer -> Integer -> Integer
modPowLowLevel a b m
  -- Corner case
  | b == 0                = 1
  -- Ensure 0 <= a < m
  | a >= m                = modPowLowLevel (a `rem` m) b m
  -- Base case
  | b == 1                = a
  -- Recursive cases
  | even b                = square (modPowLowLevel a b' m) `rem` m
  | otherwise             = (a * square (modPowLowLevel a b' m)) `rem` m
                                where b' = b `quot` 2

{-|
    @egcd m n@ is the extended GCD of @m@ and @n@. It is a tuple @(g, (a, b))@
    such that @gcd m n == g@ and @a*m + b*n == g@.

    __Preconditions:__

      * None

    >>> egcd 3 5
    (1, (2, -1))
    >>> egcd 10 4
    (2, (1, -2))
    >>> egcd (-4) 7
    (1, (-2, -1))
    >>> egcd 4 0
    (4, (1, 0))
    >>> egcd 0 0
    (0, (0, 0))
-}
egcd :: (Integral a) => a -> a -> (a, (a, a))
egcd m n
  | m < n  = (g', (signa * a', signb * b'))
  | otherwise = (g', (signb * b', signa * a'))
  where (g', (a', b')) = egcdLowLevel (abs lower) (abs upper)
        signa = signum lower
        signb = signum upper
        lower = min m n
        upper = max m n

{- Here, we are guaranteed n > m > 0 -}
egcdLowLevel :: (Integral a) => a -> a -> (a, (a, a))
egcdLowLevel m n = egcdRecursive m n (1, 0) (0, 1)

egcdRecursive :: (Integral a) => a -> a -> (a, a) -> (a, a) -> (a, (a, a))
egcdRecursive m n m_coeffs n_coeffs
    | m == 0 = (n, n_coeffs)
    | otherwise = egcdRecursive r m (n0 - q*m0, n1 - q*m1) m_coeffs
  where
    (q, r) = quotRem n m
    (m0, m1) = m_coeffs
    (n0, n1) = n_coeffs

{-|
    @modInv a m@ is the modular inverse of @a@ modulo @m@, if it exists.

    __Preconditions:__

      * None

    >>> modInv 3 5
    Just 2
    >>> modInv 2 6
    Nothing
    >>> modInv 3 (-5)
    Just 2
    >>> modInv 16 7
    Just 4
    >>> modInv (-5) 1
    Just 1
-}
modInv :: (Integral a) => a -> a -> Maybe a
modInv a m = if g == 1 then Just b' else Nothing
  where (g, (b, _)) = egcd a m
        b' = if b > 0 then b else b + m
