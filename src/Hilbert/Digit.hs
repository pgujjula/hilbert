module Hilbert.Digit
  ( ndigits
  , sumDigits
  , digits) where

ndigits :: (Integral a) => a -> Int
ndigits n | abs n < 10 = 1
ndigits n = 1 + (ndigits $ n `div` 10)

sumDigits :: (Integral a) => a -> a
sumDigits n | n < 10 = n
sumDigits n = (n `rem` 10) + sumDigits (n `div` 10)

digits :: (Integral a) => a -> [Int]
digits = reverse . digits'

digits' :: (Integral a) => a -> [Int]
digits' n | n < 10 = [fromIntegral n]
digits' n = ((fromIntegral n) `rem` 10):(digits' (n `div` 10))
