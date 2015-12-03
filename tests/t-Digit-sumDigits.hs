module Main where

import System.Exit (exitFailure, exitSuccess)
import System.Random
import Data.Char (ord)

import Hilbert.Digit (sumDigits)

-- The number of tests to run.
ntests = 10

-- Note that this is *not* the digits from Hilbert.Digit
digits n = map ((\x -> x - (ord '0')) . ord) $ show n

range = (0, 10^500)

test :: Integer -> Bool
test n = (sumDigits n) == (fromIntegral $ sum $ digits n)

genCases :: (RandomGen g) => g -> [Integer]
genCases gen = [0..100] ++ (take ntests $ randomRs range gen)

errorMsg :: Integer -> String
errorMsg n = "ERROR: In computing sumDigits " ++ (show n) ++ "\n"
          ++ "  expected: " ++ (show $ sum $ digits n) ++ "\n"
          ++ "  but got : " ++ (show $ sumDigits n) ++ "\n"

main = do
  gen <- getStdGen
  let cases = genCases gen
  let wrongs = filter (not . test) cases
  if null wrongs
  then exitSuccess
  else do
    sequence_ $ map (putStrLn . errorMsg) wrongs
    exitFailure

