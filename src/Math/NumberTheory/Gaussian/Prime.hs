-- | Module      : Math.NumberTheory.Gaussian.Prime
--   Description : Functions related to Gaussian primes.
--   Copyright   : (c) Preetham Gujjula, 2024
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Functions related to Gaussian primes.
module Math.NumberTheory.Gaussian.Prime
  ( isPrime,
    primes,
  )
where

import Data.Complex (Complex ((:+)), realPart)

-- | Whether a Gaussian integer is prime.
isPrime :: (Integral a) => Complex a -> Bool
isPrime n = undefined (realPart n + 1)

-- | An infinite list of the Guassian primes. Primes are ordered by norm, then
-- by polar angle.
primes :: (Integral a) => [Complex a]
primes = [undefined :+ 0]

-- | Enumerate the Gaussian integers, ordered by norm
--enumerate :: (Integral a) => [Complex a]
--enumerate =
--  let xs = 0 : concatMap (\i -> [-i, i]) [1..]
--   in applyMergeBy (comparing norm) (:+) xs xs
