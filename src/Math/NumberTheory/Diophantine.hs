{-# LANGUAGE ScopedTypeVariables #-}

-- | Module      : Math.NumberTheory.Diophantine
--   Description : Solve diophantine equations
--   Copyright   : (c) Preetham Gujjula, 2020
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Solve diophantine equations.
module Math.NumberTheory.Diophantine
  ( solvePell,
    pythagoreanTriples,
    primitivePythagoreanTriples,
  )
where

import Data.List (find)
import Data.List.Merge (applyMerge)
import Data.List.Ordered (mergeAll)
import Data.Maybe (fromJust)
import Data.Ratio (denominator, numerator)
import Math.NumberTheory.ContinuedFraction as CF
import Math.NumberTheory.Power (isSquare, square)

-- | @solvePell d@ yields all solutions (x, y) in positive integers to the Pell
--   equation x² - d y² = 1 in ascending order.
--
--   __Precondition:__ @d >= 1@. If @d <= 0@, then no solutions will be reported,
--       even if the resulting (non-Pell) equation has solutions.
--
--   >>> (take 3) <$> (solvePell 2)
--   [(3, 2), (17, 20), (99, 70)]
--   >>> (take 3) <$> (solvePell 4)
--   []
solvePell :: (Integral a) => a -> [(a, a)]
solvePell d
  | d < 1 = []
  | d == 1 = [(1, 0)]
  | isSquare d = []
  | otherwise = scanl1 combine (repeat fundamental)
  where
    -- Get the fundamental solution (x, y) to the Pell equation x² - d y² = 1
    --
    -- Preconditions:
    -- \* @d >= 2@
    -- \* @d@ is not a square.
    fundamental =
      toPair $
        fromJust $
          find solution $
            map (convergent (CF.sqrt d)) [1 ..]
      where
        solution frac = square (numerator frac) - d * square (denominator frac) == 1
        toPair frac = (numerator frac, denominator frac)

    -- Combine two solutions (x₁, y₁) and (x₂, y₂) to the Pell equation with
    -- parameter d, to make another solution (x₃, y₃), where
    --
    -- (x₁ + y₁ √d) * (x₂ + y₂ √d) == (x₃ + y₃ √d)
    combine (x1, y1) (x2, y2) =
      ( x1 * x2 + y1 * y2 * d,
        x1 * y2 + x2 * y1
      )

newtype Triple a = Triple {unTriple :: (a, a, a)}
  deriving (Show)

instance Eq a => Eq (Triple a) where
  Triple x == Triple y = x == y

instance Ord a => Ord (Triple a) where
  compare (Triple (a, b, c)) (Triple (a', b', c')) = compare (c, a, b) (c', a', b')

scale :: (Integral a) => Triple a -> a -> Triple a
scale (Triple (a, b, c)) k = Triple (k * a, k * b, k * c)

-- | List of all integer triples (a, b, c) such that
--
--   \[a^2 + b^2 = c^2\]
--   \[0 < a \le b \le c\]
--
--   Ordered by increasing c, and then by increasing b.
pythagoreanTriples :: (Integral a) => [(a, a, a)]
pythagoreanTriples =
  map unTriple $
    applyMerge scale (map Triple primitivePythagoreanTriples) [1 ..]

-- | List of all integer triples (a, b, c) such that
--   \[a^2 + b^2 = c^2\]
--   \[0 < a \le b \le c\]
--   \[\text{gcd}(a, b, c) = 1\]
--   Ordered by increasing c, and then by increasing a.
primitivePythagoreanTriples :: forall a. (Integral a) => [(a, a, a)]
primitivePythagoreanTriples = map unTriple $ mergeAll tripleLists
  where
    -- We use Euclid's formula to generate Pythagorean triples. For each primitive
    -- triple a, b, c and b even, there are m, n with m > n > 0, such that m and n
    -- are not both odd, and
    --     a = m^2 - n^2,  b = 2*m*n,  c = m^2 + n^2
    -- In this formula b is always even, but we want a <= b. So we swap a and b if
    -- we have to.
    mkTriple :: (a, a) -> Triple a
    mkTriple (m, n) = Triple (a, b, c)
      where
        a' = m2 - n2
        b' = 2 * m * n
        (a, b) = if a' <= b' then (a', b') else (b', a')
        c = m2 + n2

        m2 = square m
        n2 = square n

    -- get all possible values of n that could be paired with m to make a primitive
    -- pythagorean triple, subject to the conditions above
    getns :: a -> [a]
    getns m = filter (\n -> oddFilter n && gcd m n == 1) [1 .. m - 1]
      where
        oddFilter x = even m || even x

    -- A list of lists of all primitive triples. tripleLists !! i is a list of all
    -- triples such that m == i (and such that the other conditions above hold).
    tripleLists :: [[Triple a]]
    tripleLists = map (map mkTriple . mkRow) [0 ..]
      where
        mkRow i = zip (repeat i) (getns i)
