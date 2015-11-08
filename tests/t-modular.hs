module Main where

import System.Exit (exitFailure, exitSuccess)
import Hilbert.Modular
import System.Random

main = do
  c <- cases
  let success = filter (not . test) c
  if null success
  then exitSuccess
  else do
    putStrLn $ show $ success
    exitFailure

-- Generate small test cases (0 < b, e, m < 100)
cases :: IO [(Integer, Integer, Integer)]
cases = do
  generator <- getStdGen
  let list = randomRs (1, 100) generator
  let (bs, rest1) = splitAt 10 list
  let (es, rest2) = splitAt 10 rest1
  let (ms, rest3) = splitAt 10 rest2
  return $ zip3 bs es ms

test :: (Integer, Integer, Integer) -> Bool
test (b, e, m) = (modPow b e m) == (b^e) `mod` m
