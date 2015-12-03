module Main where

import System.Exit (exitFailure, exitSuccess)
import System.Random
import Data.Char (ord)

import qualified Hilbert.Digit as Digit (digits)

-- The number of tests to run.
ntests = 10

-- Note that this is *not* the digits from Hilbert.Digit
digits n = map ((\x -> x - (ord '0')) . ord) $ show n

range = (0, 10^500)

test :: Integer -> Bool
test n = (digits n) == (Digit.digits n)

genCases :: (RandomGen g) => g -> [Integer]
genCases gen = [0..100] ++ (take ntests $ randomRs range gen)

errorMsg :: Integer -> String
errorMsg n = "ERROR: In computing digits " ++ (show n) ++ "\n"
          ++ "  expected: " ++ (show $ digits n) ++ "\n"
          ++ "  but got : " ++ (show $ Digit.digits n) ++ "\n"

main = do
  gen <- getStdGen
  let cases = genCases gen
  let wrongs = filter (not . test) cases
  if null wrongs
  then exitSuccess
  else do
    sequence_ $ map (putStrLn . errorMsg) wrongs
    exitFailure
