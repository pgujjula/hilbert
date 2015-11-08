module Main where

import System.Exit (exitFailure, exitSuccess)
import Hilbert.Prime (trialDivision)

-- Ensure that trialDivision works for the first
-- 1000 natural numbers by comparing it to primeDefinition,
-- a very simple primality test.

-- Super simple primality test that uses the mathematical
-- definition of primality: A prime number has exactly two
-- factors
primeDefinition :: (Integral a) => a -> Bool
primeDefinition n = (length $ filter (`divides` n) [1..n]) == 2
  where divides a b = b `rem` a == 0

-- Range of numbers to test
limit = 100
range = [1..limit]

-- List of primes as computed by primeDefinition
primeDefinitionList = filter primeDefinition range

-- List of primes as computed by trialDivision
trialDivisionList = filter trialDivision range

main = if primeDefinitionList == trialDivisionList
       then exitSuccess
       else do
         putStr $ "Expected:\n"
                  ++ "\t" ++ show primeDefinitionList ++ "\n"
                  ++ "but got" ++ "\n"
                  ++ "\t" ++ show trialDivisionList ++ "\n"
                  ++ "for primes less than " ++ show limit ++ ".\n"
         exitFailure
