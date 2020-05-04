{-| Module      : Math.NumberTheory.Diophantine
    Description : Solve diophantine equations
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

Solve diophantine equations.
-}
module Math.NumberTheory.Diophantine
    ( solvePell
    ) where

import Math.NumberTheory.ContinuedFraction as CF
import Math.NumberTheory.Power (isSquare)

import Data.Ratio (numerator, denominator)
import Data.Maybe (fromJust)
import Data.List (find)

{-|
    @solvePell d@ yields all solutions (x, y) in positive integers to the Pell
    equation x² - d y² = 1 in ascending order.

    __Precondition:__ @d >= 1@. If @d <= 0@, then no solutions will be reported,
        even if the resulting (non-Pell) equation has solutions.

    >>> (take 3) <$> (solvePell 2)
    [(3, 2), (17, 20), (99, 70)]
    >>> (take 3) <$> (solvePell 4)
    []
-}
solvePell :: (Integral a) => a -> [(a, a)]
solvePell d
    | d < 1      = []
    | d == 1     = [(1, 0)]
    | isSquare d = []
    | otherwise  = scanl1 combine (repeat fundamental)
  where
    {- Get the fundamental solution (x, y) to the Pell equation x² - d y² = 1   

        __Preconditions:__
            * @d >= 2@
            * @d@ is not a square.
    -}
    fundamental = toPair $ fromJust $ find solution
                $ map (convergent (CF.sqrt d)) [1..]
      where solution frac = (numerator frac)^2 - d * (denominator frac)^2 == 1
            toPair frac = (numerator frac, denominator frac)

    {- Combine two solutions (x₁, y₁) and (x₂, y₂) to the Pell equation with
       parameter d, to make another solution (x₃, y₃), where

           (x₁ + y₁ √d) * (x₂ + y₂ √d) == (x₃ + y₃ √d)
    -}
    combine (x1, y1) (x2, y2) = (x1 * x2 + y1 * y2 * d, 
                                 x1 * y2 + x2 * y1)
