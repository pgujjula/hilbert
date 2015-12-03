-- Test ContinuedFrac.convergent by ensuring that 
-- convergents for a square root actually approach a square root.
module Main where

import System.Exit (exitFailure, exitSuccess)
import System.Random

import Data.List (sort)

import Hilbert.ContinuedFrac (convergent, cfracSqrt)
import Hilbert.Square (isSquare)

-- Compute the infinite continued fraction expansion
-- for sqrt n
toInfinite :: (Integral a) => a -> [a]
toInfinite n = a:(cycle b)
  where Just (a, b) = cfracSqrt n

-- Check that convergents approach the target square root
test n = (reverse $ sort approximations) == approximations
 where
  list = [1..limit]
  limit = 5
  Just (a, b) = cfracSqrt n
  cfrac = a:(cycle b)
  approximations = map (abs . ((sqrt (fromIntegral n)) - ) . fromRational
                          . (convergent cfrac)) list

-- Generate test cases for all nonsquares less than 1000.
cases :: [Integer]
cases = filter (not . isSquare) [1..1000]

-- The failed test cases
wrongs :: [Integer]
wrongs = filter (not . test) cases

errorMsg :: Integer -> String
errorMsg n = 
  "ERROR: The continued fraction convergents for " ++ (show n)
  ++ " don't approach sqrt(" ++ (show n) ++ ").\n"

main = if null wrongs
       then exitSuccess
       else do
         sequence_ $ map (putStrLn . errorMsg) $ wrongs
         exitFailure
