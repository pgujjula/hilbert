module Hilbert.Prime
  ( isPrime
  , trialDivision
  , millerRabin
  , lucasLehmer
  , bailliePSW
  , primes
  , factor
  , factorizations) where

import Data.List  (find, foldl', intercalate)
import Data.Maybe (fromJust)

import Hilbert.Prime.MillerRabin   (millerRabin)
import Hilbert.Prime.LucasLehmer   (lucasLehmer)
import Hilbert.Prime.BailliePSW    (bailliePSW)
import Hilbert.Prime.TrialDivision (trialDivision)
import Hilbert.Prime.List          (primes)
import Hilbert.Prime.Factor        (factor, factorizations)

import Hilbert.Legendre (jacobi)
import Hilbert.Square (isSquare)
import Hilbert.Modular (modPow)
import Hilbert.Lucas
import System.Random
import Data.Ratio
import qualified Data.Map as Map

isPrime :: (Integral a) => a -> Bool
isPrime n
  | n <= 100 = trialDivision n
  | otherwise = isPrime1 n

genvsch n = zip (map (\n -> ((u_seq 1 (-1)) !! n) `rem` 101) (path n))
                (map (\n -> ((v_seq 1 (-1)) !! n) `rem` 101) (path n))

primesUnder100 :: (Integral a) => [a]
primesUnder100 = [ 2,  3,  5,  7, 11,
                  13, 17, 19, 23, 29,
                  31, 37, 41, 43, 47,
                  53, 59, 61, 67, 71,
                  73, 79, 83, 89, 97]

--Precondition : n >= 100
isPrime1 :: (Integral a) => a -> Bool
isPrime1 n = (not $ any (\r -> n `rem` r == 0)
                       primesUnder100)
          && bailliePSW n
