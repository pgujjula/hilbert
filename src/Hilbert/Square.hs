module Hilbert.Square where

import Hilbert.Digit (numDigits)
import Data.Maybe (fromJust)
import Data.List (find)

{-| Returns true if the input is a perfect square. -}
-- Algorithm: Create better approximations for sqrt(n) using Newton's method.
-- If x is an approximation for sqrt(n), then (x*x + n)/(2*x) is
-- a better approximation.
isSquare n =
  let -- The initial approximation
      initial = 10^((numDigits n) `div` 2)

      -- A list of improving approximations
      list = iterate initial
      iterate x = x:(iterate next)
        where next = (x*x + n) `div` (2*x)

      -- Find the first approximation that does not result in
      -- a better approximation, and call it potentialroot.
      differences = zipWith (\a b -> abs (a - b)) list (tail list)
      listwithdiffs = zip list differences
      potentialroot = fst $ fromJust $ find (\(a, b) -> b <= 1) listwithdiffs

      -- potentialroot must be at most 1 off from the actual square
      -- root, if it exists.
   in (potentialroot^2 == n)
      || (potentialroot + 1)^2 == n
      || (potentialroot - 1)^2 == n
