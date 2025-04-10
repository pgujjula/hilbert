-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Math.NumberTheory.Gaussian.Divisor
--   Description : Gaussian divisors of Gaussian integers.
--   Copyright   : (c) Preetham Gujjula, 2024
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Gaussian divisors of Gaussian integers.
module Math.NumberTheory.Gaussian.Divisor
  ( -- * Basic functions
    divides,
    relativelyPrime,

    -- * Listing divisors
    divisors,
    divisorsF,
    divisorPairs,
    divisorPairsF,

    -- * Summarizing divisors
    numDivisors,
    numDivisorsF,
  )
where

import Data.Complex (Complex, realPart)
import Math.NumberTheory.Prime.Factor (Factorization)

-- | @a \`divides\` b@ is @True@ iff there is a Gaussian integer @k@ such that
-- @a*k == b@.
divides :: (Integral a) => Complex a -> Complex a -> Bool
divides a b = undefined (realPart a + 1) (realPart b)

-- | @relativelyPrime m n@ is @True@ if @m@ and @n@ have no common positive
-- divisors other than 1.
relativelyPrime :: (Integral a) => Complex a -> Complex a -> Bool
relativelyPrime a b = undefined (realPart a + 1) (realPart b + 1)

-- | The divisors of a Gaussian integer. Not necessarily in sorted order.
divisors :: (Integral a) => Complex a -> [Complex a]
divisors n = undefined (realPart n + 1)

-- | The positive divisors of a Gaussian integer, from its factorization. Not
-- necessarily in sorted order.
divisorsF :: (Integral a) => Factorization (Complex a) -> [Complex a]
divisorsF xs = undefined (realPart (fst (head xs)) + 1)

-- | The divisor pairs of a Gaussian integer @n@, paired up such that every
-- pair @(a, b)@ satisfies @a*b == n@.
divisorPairs :: (Integral a) => Complex a -> [(Complex a, Complex a)]
divisorPairs x = undefined (realPart x + 1)

-- | The divisors of a Gaussian integer n, from its prime factorization, paired
-- up such that every pair @(a, b)@ satisfies @a*b == n@. Not in any particular
-- order.
divisorPairsF ::
  (Integral a) => Factorization (Complex a) -> [(Complex a, Complex a)]
divisorPairsF x = undefined (realPart (fst (head x)) + 1)

-- | The number of divisors of a Gaussian integer @n@.
numDivisors :: Integral a => Complex a -> a
numDivisors n = undefined (realPart n + 1)

-- | The number of divisors of a Gaussian integer @n@, from its prime
-- factorization.
numDivisorsF :: Integral a => Factorization (Complex a) -> a
numDivisorsF xs = undefined (realPart (fst (head xs)) + 1)
