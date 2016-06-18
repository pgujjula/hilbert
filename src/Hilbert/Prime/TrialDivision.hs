{-|
    Module      : Hilbert.Prime.TrialDivision
    Description : Functions to compute the primality of a number using
                  trial division.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Functions to compute the primality of a number using trial division.
-}
module Hilbert.Prime.TrialDivision
  ( trialDivision
  ) where

{-|
    Determines whether n is prime by testing
    potential factors up to the square root of n.
-}
trialDivision :: (Integral a) => a -> Bool
trialDivision n
  | n < 2 = False -- Anything less than 2 is not prime.
  | n < 4 = True  -- 2 and 3 are prime.
  | otherwise = all (\a -> n `rem` a /= 0) testFactors
         where testFactors = 2:[3, 5..upperLimit]
               upperLimit = floor $ sqrt $ fromIntegral n
