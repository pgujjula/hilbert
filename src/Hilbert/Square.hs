{-|
Module      : Hilbert.Square
Description : Functions related to squaring.
Copyright   : (c) Preetham Gujjula, 2016
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental

Functions related to squaring.
-}

module Hilbert.Square where

import Hilbert.Digit (numDigits)
import Data.Maybe (fromJust)
import Data.List (find)

{-| @isSquare n@ returns whether the @n@ is a perfect square.

    > Preconditions: none

    >>> isSquare 4
    True
    >>> isSquare 11
    False
    >>> isSquare 0
    True
-}

{- Algorithm: Create better approximations for sqrt(n) using Newton's method.
   If x is an approximation for sqrt(n), then (x*x + n)/(2*x) is
   a better approximation.
-}

isSquare :: Integral a => a -> Bool
isSquare n =
  let -- The initial approximation
      initial = 10^((numDigits n) `div` 2)

      -- A list of improving approximations
      approximations = iterate initial
      iterate x = x:(iterate next)
        where next = (x*x + n) `div` (2*x)

      -- Find the first approximation that does not result in
      -- a better approximation, call it potentialroot.
      differences = zipWith (\a b -> abs (a - b)) approximations (tail approximations)
      approxWithDiffs = zip approximations differences
      potentialroot = fst $ fromJust $ find (\(a, b) -> b <= 1) approxWithDiffs

      -- potentialroot must be at most 1 off from the actual square
      -- root, if it exists.
   in (potentialroot^2 == n)
      || (potentialroot + 1)^2 == n
      || (potentialroot - 1)^2 == n
