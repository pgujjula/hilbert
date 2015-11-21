module Hilbert.Pell (pellSolve, minimalSolution) where

import Data.Maybe (fromJust)
import Data.Ratio
import Hilbert.ContinuedFrac (convergent, continuedFrac)
import Hilbert.Square (isSquare)

pellSolve :: (Integral a) => a -> [(a, a)]
pellSolve d
  | isSquare d = [(1, 0)]
  | otherwise = (1, 0):rest
  where rest = genSols d minim
        minim = minimalSolution d
        
mul d (x0, y0) (xi, yi) = (x0*xi + y0*yi*d, x0*yi + y0*xi)
genSols d (x0, y0)= gs
  where gs = (x0, y0):(map (mul d (x0, y0)) gs)

minimalSolution d = (denom, numer)
  where (numer, denom) = (\r -> (numerator r, denominator r)) frac
        frac
          | odd l    = convergent cfrac (2*(fromIntegral l))
          | otherwise = convergent cfrac ((fromIntegral l))
        l = period d
        genSolutions = undefined
        cfrac = (\(a, b) -> a:(cycle b)) $ fromJust $ continuedFrac d

expand (Just (a, b)) = a:(cycle b)
period d = length $ snd $ fromJust $ continuedFrac d
