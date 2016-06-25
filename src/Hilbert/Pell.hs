{-|
    Module      : Hilbert.Pell
    Description : Solve the Pell equation
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Solve the Pell equation x² - d y² = 1.
-}

module Hilbert.Pell
  ( solve
  ) where

import Hilbert.ContinuedFraction as CF
import Hilbert.Square (isSquare)
import Data.Ratio
import Data.Maybe (fromJust)
import Data.List (find)

{-
    Get the fundamental solution (x, y) to the Pell equation x² - d y² = 1   

    >>> fundamentalSolution 2
    Just (3, 2)
    >>> fundamentalSolution 4
    Nothing
-}
fundamentalSolution :: (Integral a) => a -> Maybe (a, a)
fundamentalSolution d
  | d < 2      = Nothing
  | isSquare d = Nothing
  | otherwise  = Just $ fundamentalSolution_unchecked d

-- Get the fundamental solution without checking preconditions.
fundamentalSolution_unchecked :: (Integral a) => a -> (a, a)
fundamentalSolution_unchecked d = 
  toPair $ fromJust $ find solution $ map (convergent (CF.sqrt d)) [1..]
  where solution frac = (numerator frac)^2 - d * (denominator frac)^2 == 1
        toPair frac = (numerator frac, denominator frac)

{-|
    Get all solutions (x, y) in positive integers to the Pell equation
    x² - d y² = 1 in ascending order

    >>> take 3 (solutions 2)
    Just [(3, 2), (17, 20), (99, 70)]
    >>> take 3 (solutions 4)
    Nothing
-}
solve :: (Integral a) => a -> Maybe [(a, a)]
solve d | d < 2 = Nothing
solve d | isSquare d = Nothing
solve d = Just $ scanl (combine d) fs (repeat fs)
  where fs = fundamentalSolution_unchecked d

{-
    Combine two solutions (x₁, y₁) and (x₂, y₂) to the Pell equation with
    parameter d, to make another solution (x₃, y₃), where

        (x₁ + y₁ √d) * (x₂ + y₂ √d) == (x₃ + y₃ √d)
-}
combine :: (Integral a) => a -> (a, a) -> (a, a) -> (a, a)
combine d (x1, y1) (x2, y2) = (x1 * x2 + y1 * y2 * d, 
                               x1 * y2 + x2 * y1)
