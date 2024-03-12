-- | Module      : Math.NumberTheory.Gaussian.Prime.Factor
--   Description : Factorization into Gaussian primes.
--   Copyright   : (c) Preetham Gujjula, 2024
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Factorization into Gaussian primes.
module Math.NumberTheory.Gaussian.Prime.Factor (factor, factorizations) where

import Data.Complex (Complex, realPart)
import Math.NumberTheory.Prime.Factor (Factorization)

-- dummy implementation to get rid of warnings
-- | Factor a Gaussian integer
factor :: (Integral a) => Complex a -> Factorization (Complex a)
factor n = undefined (fromIntegral (realPart n) :: Int)

-- | Factorizations of the Gaussian integers. Integers are ordered by norm, and
-- then by polar angle.
factorizations :: [Factorization (Complex Int)]
factorizations = undefined
