module Hilbert.Square where

import Hilbert.Digit (ndigits)
import Data.Maybe (fromJust)
import Data.List (find)

isSquare n =
  let initial = 10^((ndigits n) `div` 2)
      list = iterate initial
      iterate x = x:(iterate next)
        where next = (x*x + n) `div` (2*x)
      differences = zipWith (\a b -> abs (a - b)) list (tail list)
      listwithdiffs = zip list differences
      potentialroot = fst $ fromJust $ find (\(a, b) -> b <= 1) listwithdiffs
   in (potentialroot^2 == n)
      || (potentialroot + 1)^2 == n
      || (potentialroot - 1)^2 == n
      
