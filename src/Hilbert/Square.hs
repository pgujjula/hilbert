{-|
Module      : Hilbert.Square
Description : Functions related to squaring.
Copyright   : (c) Preetham Gujjula, 2016
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental

Functions related to squaring.
-}

module Hilbert.Square
     ( integralSqrt
     , isSquare
     ) where

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

{- Generate potential square roots using Newton's method. 
   If x is an approximation for sqrt(n), then (x*x + n)/(2*x) is
   a better approximation.
-}
potentialRoots :: Integral a => a -> [a]
potentialRoots m = 
  let -- To deal with fixed precision integers, we need to convert to
      -- Integer first.
      n = toInteger m

      -- The initial approximation
      initial = 10^((numDigits n) `div` 2)

      -- A list of improving approximations
      approximations = iterate initial
      iterate x = x:(iterate next)
        where next = (x*x + n) `div` (2*x)

      -- Find the first approximation that does not result in
      -- a better approximation, call it potentialroot.
      differences = zipWith (\a b -> abs (a - b)) approximations (tail approximations)
      approxWithDiffs = zip approximations differences
      potentialRoot = fst $ fromJust $ find (\(a, b) -> b <= 1) approxWithDiffs

      -- The actual square root must be at most 1 off from the actual square
   in map fromIntegral
        [potentialRoot - 1, potentialRoot, potentialRoot + 1]

isSquare :: Integral a => a -> Bool
isSquare n = any (== n) $ map (^2) $ potentialRoots n

{-| @integralSqrt n@ computes the largest integer less than
    the square root of @n@.
-}
integralSqrt :: Integral a => a -> a
integralSqrt n =
  let pr = potentialRoots n
      potentialRoots' = reverse $ [(head pr) - 1] ++ pr ++ [(last pr) + 1]
   in fromJust $ find (\x -> (x^2) <= n) potentialRoots'
