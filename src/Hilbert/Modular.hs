{-|
    Module      : Hilbert.Modular
    Description : Functions to perform modular arithmetic.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Functions to perform modular arithmetic.
-}
module Hilbert.Modular
    ( modPow
    ) where

{-|
    @modPow a b m @ efficiently computes @mod (a^b) m@.

    > Preconditions:
    >   a >= 0
    >   b >= 0
    >   m > 0

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
modPow a b m = fromIntegral $ modPow_low_level (toInteger a) (toInteger b) (toInteger m)

modPow_low_level :: Integer -> Integer -> Integer -> Integer
modPow_low_level a b m
-- Corner case
  | b == 0                = 1
-- Ensure 0 <= a < m
  | a >= m                = modPow (a `rem` m) b m
-- Base case
  | b == 1                = a
-- Recursive cases
  | b `rem` 2 == 0        = (modPow a b' m)^2 `rem` m
  | otherwise             = (a * (modPow a b' m)^2) `rem` m
                                where b' = b `quot` 2
