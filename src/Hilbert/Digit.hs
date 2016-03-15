{-|
Module      : Hilbert.Modular
Description : Functions to perform modular arithmetic.
Copyright   : (c) Preetham Gujjula, 2016
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental

Functions to perform handle the digits of integers.
-}

module Hilbert.Digit
  ( numDigits
  , sumDigits
  , digits) where

{- | @numDigits n@ is the number of digits in @n@.

   > Precondition: n >= 0

   >>> numDigits 2938475
   7
   >>> numDigits 0
   1
-}
numDigits :: (Integral a) => a -> Int
numDigits n | abs n < 10 = 1
numDigits n = 1 + (numDigits $ n `div` 10)

{- | @sumDigits n@ is the sum of the digits in @n@.

   > Precondition: n >= 0

   >>> sumDigits 2938475
   38
   >>> sumDigits 0
   0
-}
sumDigits :: (Integral a) => a -> a
sumDigits n | n < 10 = n
sumDigits n = (n `rem` 10) + sumDigits (n `div` 10)

{- | @digits n@ is a list of all the digits in @n@. 

   > Precondition: n >= 0

   >>> digits 2938475
   [2, 9, 3, 8, 4, 7, 5]
   >>> digits 0
   [0]
-}
digits :: (Integral a) => a -> [Int]
digits = reverse . digits'
  where
  -- Generate the digits in reverse order, which is easier.
    digits' :: (Integral a) => a -> [Int]
    digits' n | n < 10 = [fromIntegral n]
    digits' n = first:rest
      where first = fromIntegral $ n `rem` 10
            rest = digits' (n `div` 10)
