module Main where

import System.Exit (exitFailure, exitSuccess)
import System.Random
import Data.List ((\\), sort)

import Hilbert.List (rmDups)
import Hilbert.Prime (millerRabin, trialDivision)

-- Ensure that millerRabin works by comparing it to
-- the result of trialDivision.

ncases = 10000
limit = 1000000

-- Numbers to test
genCases :: (RandomGen g) => g -> [Int]
genCases = (take ncases) . (randomRs (1, limit))

-- List of primes as computed by trialDivision
trialDivisionList g = filter trialDivision (genCases g)

-- List of primes as computed by millerRabin
bases = [2, 3, 5, 7]
millerRabinList g = (filter (\n -> if n == 1 then False
                                   else millerRabin n bases
                                     || n `elem` bases)
                           (genCases g))


main = do
  g <- getStdGen
  let cases = genCases g
  let tdl = trialDivisionList g
  let mrl = millerRabinList g
  if tdl == mrl
  then exitSuccess
  else do
    putStr $ "Expected but not included: "
          ++ "\t" ++ show (rmDups $ sort $ tdl\\mrl) ++ "\n"
          ++ "Included but not expected: "
          ++ "\t" ++ show (rmDups $ sort $ mrl\\tdl) ++ "\n"
    exitFailure
