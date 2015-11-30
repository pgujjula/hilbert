module Main where

import System.Exit (exitFailure, exitSuccess)
import System.Random
import Data.List (sort)

import Hilbert.Digit
import Hilbert.List (rmDups)
-- The number of tests to run
ntests = 10000

-- An list of test cases of the form (d, *number with d digits*)
genTests :: (RandomGen g) => g -> [(Int, Integer)]
genTests gen = (digits, num):rest
  where (digits, gen1) = randomR (1, 100) gen
        (num,    gen2) = randomR (10^(digits - 1), 10^digits - 1) gen1
        rest = genTests gen2

testAll :: (RandomGen g) => g -> [(Integer, Int, Int)]
testAll = (map testFunc) . (take ntests) . genTests
  where testFunc (answer, number) = (number, answer, result)
          where result = ndigits number

wrongAnswers :: (RandomGen g) => g -> [(Integer, Int, Int)]
wrongAnswers = rmDups . sort . (filter filterFunc) . testAll
  where filterFunc (input, expected, actual) = expected /= actual

errorMsgs :: (RandomGen g) => g -> [String]
errorMsgs = (map errorMsg) . wrongAnswers
  where errorMsg (input, expected, actual) = 
            "expected: ndigits " ++ (show input) ++ " == " ++ (show expected) ++ "\n"
         ++ "but got : ndigits " ++ (show input) ++ " == " ++ (show actual)   ++ "\n"

main = do
  g <- getStdGen
  let errors = errorMsgs g
  if null errors
  then exitSuccess
  else do
    sequence_ $ map putStrLn errors
    exitFailure
