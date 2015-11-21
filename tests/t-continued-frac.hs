module Main where

import System.Exit (exitFailure, exitSuccess)
import Hilbert.ContinuedFrac

-- The continued fractions of sqrt(1), sqrt(2), ..., sqrt(10)
-- courtesy of Wolfram Alpha.
rightAnswers :: [Maybe (Int, [Int])]
rightAnswers = [Nothing,
                Just (1, [2]),
                Just (1, [1, 2]),
                Nothing,
                Just (2, [4]),
                Just (2, [2, 4]),
                Just (2, [1, 1, 1, 4]),
                Just (2, [1, 4]),
                Nothing,
                Just (3, [6])]

testAnswers :: [Maybe (Int, [Int])]
testAnswers = map continuedFrac [1..10]

main = do
  if rightAnswers == testAnswers
  then exitSuccess
  else exitFailure
