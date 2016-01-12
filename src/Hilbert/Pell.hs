module Hilbert.Pell (pellSolve, minimalSolution) where

import Data.Maybe (fromJust)
import Data.Ratio
import Hilbert.ContinuedFrac (convergent, cfracSqrt)
import Hilbert.Square (isSquare)

{-| @pellSolve n @ returns all solutions @(x, y)@ to the equation
    @x^2 - n*y^2 == 1@ -}
pellSolve :: (Integral a) => a -> [(a, a)]
pellSolve d
  | isSquare d = [(1, 0)]
  | otherwise = (1, 0):rest
  where rest = genSols d minim
        minim = minimalSolution d

mul :: (Integral a) => a -> (a, a) -> (a, a) -> (a, a)
mul d (x0, y0) (xi, yi) = (x0*xi + y0*yi*d, x0*yi + y0*xi)

genSols :: (Integral a) => a -> (a, a) -> [(a, a)]
genSols d (x0, y0) = gs
  where gs = (x0, y0):(map (mul d (x0, y0)) gs)

{-| @minimalSolution n @ returns the minimal solution @(x', y')@ to the
   equation @x^2 - n*y^2 == 1@ -}
minimalSolution :: (Integral a) => a -> (a, a) 
minimalSolution d = (numer, denom)
  where (numer, denom) = (\r -> (numerator r, denominator r)) frac
        frac
          | odd l    = convergent cfrac (2*(fromIntegral l))
          | otherwise = convergent cfrac ((fromIntegral l))
        l = period d
        genSolutions = undefined
        cfrac = (\(a, b) -> a:(cycle b)) $ fromJust $ cfracSqrt d

period :: (Integral a) => a -> a
period d = fromIntegral $ length $ snd $ fromJust $ cfracSqrt d
