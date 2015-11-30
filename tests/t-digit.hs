module Main where

import System.Exit (exitFailure, exitSuccess)
import Hilbert.Digit
import System.Random

-- Produce a random positive integer with n digits.
integerWithDigits :: (RandomGen g) => Int -> g -> (Integer, g)
integerWithDigits n = randomR (low, high)
  where low = 10^(n - 1)
        high = (10^n) - 1

-- Produce random positive integers with the given numbers of digits.
integersWithDigits :: (RandomGen g) => [Int] -> g -> [Integer]
integersWithDigits [] g = []
integersWithDigits (x:xs) gen = 
  let (r, gen') = integerWithDigits x gen
      rs = integersWithDigits xs gen'
   in r:rs

-- A random infinite list of small positive integers (<= 100), 
-- representing the number of digits in other numbers. This is
-- the input to integersWithDigits.
digitList :: (RandomGen g) => g -> [Int]
digitList = randomRs (1, 100)

-- The number of tests to run
ntests = 1000

-- An list of test cases of the form (d, *number with d digits*)
genTests :: (RandomGen g) => g -> [(Int, Integer)] 
genTests gen = take ntests $ zip dlist integers
  where (gen1, gen2) = split gen
        dlist = digitList gen1
        integers = integersWithDigits dlist gen2

testAll :: [(Int, Integer)] -> [Bool]
testAll tests = map (\(ans, n) -> (ndigits n) == ans) tests

main = do
  g <- getStdGen
  let tests = genTests g
  let results = testAll tests
  if and results
  then exitSuccess
  else exitFailure
