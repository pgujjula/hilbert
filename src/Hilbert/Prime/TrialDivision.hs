module Hilbert.Prime.TrialDivision (trialDivision) where

-- Determines whether n is prime by testing
-- potential factors up to the square root of n.
-- Skips even factors except for 2, a small optimization.
trialDivision :: (Integral a) => a -> Bool
trialDivision n
  | n < 2 = False -- Anything less than 2 is not prime.
  | n < 4 = True  -- 2 and 3 are prime.
  | otherwise = all (\a -> n `rem` a /= 0) testFactors
         where testFactors = 2:[3, 5..upperLimit]
               upperLimit = floor $ sqrt $ fromIntegral n
