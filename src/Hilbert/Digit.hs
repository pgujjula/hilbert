-- | Functions for handling the digits of integers.
module Hilbert.Digit
  ( numDigits
  , sumDigits
  , digits) where

{- | Get the number of digits in a positive integer.
     Undefined behaviour for negative integers.

   >>> numDigits 2938475
   7
   >>> numDigits 0
   1
-}
numDigits :: (Integral a) => a -> Int
numDigits n | abs n < 10 = 1
numDigits n = 1 + (numDigits $ n `div` 10)

{- | Compute the sum of the digits of a positive integer.
     Undefined behavior for negative integers.

   >>> sumDigits 2938475
   38
   >>> sumDigits 0
   0
-}
sumDigits :: (Integral a) => a -> a
sumDigits n | n < 10 = n
sumDigits n = (n `rem` 10) + sumDigits (n `div` 10)

{- | Get the digits of an integer in base 10. Undefined behavior
     for negative integers.

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
