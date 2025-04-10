-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
-- | Module      : Math.NumberTheory.ContinuedFraction.Sqrt
--   Description : Compute the square root of an integer as a continued fraction.
--   Copyright   : (c) Preetham Gujjula, 2016
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Compute the square root of an integer as a continued fraction.
module Math.NumberTheory.ContinuedFraction.Sqrt
  ( Math.NumberTheory.ContinuedFraction.Sqrt.sqrt,
  )
where

import Math.NumberTheory.ContinuedFraction.Core
import Math.NumberTheory.Power (square)
import Math.NumberTheory.Roots (integerSquareRoot, isSquare)

-- | @sqrt n@ returns the continued fraction representation of the square root of
--   @n@.
--
--   __Precondition:__ @n@ must be nonnegative.
--
--   >>> sqrt 41
--   mkPeriodic [6] [2, 2, 12]
--   >>> sqrt 4
--   mkAperiodic [2]
--
--   To compute a good approximation for √2,
--
--   >>> fromRational $ convergent (sqrt 2) 30
--   1.4142135623730951
sqrt :: (Integral a) => a -> ContinuedFraction a
sqrt x | isSquare x = mkAperiodic [integerSquareRoot x]
sqrt x = mkPeriodic [first] periodicPart
  where
    untruncated = cfracSurd (Surd 1 x 0 1)
    first = head untruncated

    -- According to Beceanu, Theorem 2.6, the last term, l, in the periodic
    -- part of the continued fraction of a surd, and the non-repeating term
    -- f, satisfy l == 2*f. According to Myerson, this is also a sufficient
    -- condition to determine l, so we can use it to find the periodic part
    -- of the continued fraction.
    periodicPart = takeUntil (== (2 * first)) (tail untruncated)

-- Like takeWhile, but includes the first failing value.
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil proc (x1 : xs) =
  if proc x1
    then [x1]
    else x1 : takeUntil proc xs
takeUntil _ xs = xs

--  Data type to represent quadratic surds.
--  Surd a b c d represents (a sqrt(b) + c) / d.
data Surd a = Surd a a a a
  deriving (Show, Eq)

-- Cast a Surd to prevent integer overflow.
cast :: (Integral a, Integral b) => Surd a -> Surd b
cast (Surd a b c d) =
  Surd
    (fromIntegral a)
    (fromIntegral b)
    (fromIntegral c)
    (fromIntegral d)

-- Simplify a quadratic surd by removing common factors from a, c, and d. For
-- example, simplify (Surd 12 3 15 18) == Surd 4 3 5 6.
simplify :: (Integral a) => Surd a -> Surd a
simplify (Surd a b c d) = Surd a' b c' d'
  where
    (a', c', d') = (a `div` g, c `div` g, d `div` g)
    g = gcd a (gcd c d)

-- The reciprocal of a quadratic surd. We can show that the reciprocal of
-- (a sqrt(b) + c) / d is equal to (ad sqrt(b) - cd) / (a^2 * b - c^2). The
--  arguments are potentially casted to deal with overflow.
reciprocal :: (Integral a) => Surd a -> Surd a
reciprocal = cast . unsafeReciprocal . cast

-- A reciprocal that that does not handle overflow associated with fixed-
-- precision integers.
unsafeReciprocal :: Surd Integer -> Surd Integer
unsafeReciprocal (Surd a b c d) = simplify $ Surd a' b c' d'
  where
    a' = a * d
    c' = -c * d
    d' = square a * b - square c

-- Compute the floor of a quadratic surd. We have
-- * f1 = floor (a sqrt(b))
--       == integerSquareRoot (a^2 * b)
-- * f2 = floor ((a sqrt(b) + c)
--       == floor (a sqrt(b)) + c
--       == f1 + c
-- * answer = floor ((a sqrt(b)) + c) / d)
--       == floor (floor(a sqrt(b) + c) / d)
--       == floor (f2 / d)
--       == f2 `div` d
--     (see Beceanu, Lemma 2.1 for the proof of the last statement)
surdFloor :: (Integral a) => Surd a -> a
surdFloor = fromInteger . unsafeSurdFloor . cast

-- An unsafe floor that does not handle overflow associated with fixed-
-- precision integers.
unsafeSurdFloor :: (Integral a) => Surd a -> a
unsafeSurdFloor (Surd a b c d) =
  let f1 = integerSquareRoot (square a * b)
      f2 = f1 + c
   in f2 `div` d

-- Subtract an integer from a quadratic surd.
minus :: (Integral a) => Surd a -> a -> Surd a
minus (Surd a b c d) n = simplify $ Surd a b (c - d * n) d

-- Generate the continued fraction expansion of a (sqrt(b) + c) / d as a list.
-- Assumes that b is nonzero.
cfracSurd :: (Integral a) => Surd a -> [a]
cfracSurd surd = k : cfracSurd surd'
  where
    k = surdFloor surd
    surd' = reciprocal $ surd `minus` k
