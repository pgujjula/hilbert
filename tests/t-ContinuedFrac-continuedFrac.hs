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

-- The continued fractions of sqrt(1), sqrt(2), ..., sqrt(10)
-- according to Hilbert.ContinuedFrac.continuedFrac
testAnswers :: [Maybe (Int, [Int])]
testAnswers = map continuedFrac [1..10]

wrongAnswers :: [(Int, Maybe (Int, [Int]), Maybe (Int, [Int]))]
wrongAnswers = filter (\(a, b, c) -> b /= c)
             $ zip3 [1..] rightAnswers testAnswers

errorMsg :: (Int, Maybe (Int, [Int]), Maybe (Int, [Int])) -> String
errorMsg (n, exp, res) = 
     "ERROR: in computing continuedFrac " ++ (show n) ++ "\n"
  ++ "  expected: " ++ (show exp) ++ "\n"
  ++ "  but got : " ++ (show res) ++ "\n"

main = if null wrongAnswers 
       then exitSuccess
       else do
         sequence_ $ map (putStrLn . errorMsg) $ wrongAnswers
         exitFailure
