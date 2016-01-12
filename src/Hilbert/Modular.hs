module Hilbert.Modular (modPow) where

{-| @modPow b e m @ computes @b^e (mod m)@. -}
modPow :: (Integral a) => a -> a -> a -> a
modPow b e m
-- Corner case
  | e == 0                = 1
-- Ensure 0 <= b < m
  | abs b >= m || b < 0   = modPow (b `mod` m) e m
-- Base case
  | e == 1                = b
-- Recursive cases
  | e `rem` 2 == 0        = (modPow b e' m)^2 `rem` m
  | otherwise             = (b * (modPow b e' m)^2) `rem` m
                                where e' = e `div` 2
