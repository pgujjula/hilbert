{-| Module      : Math.NumberTheory.Power
    Description : Functions related to powers.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Functions related to powers.
-}

module Math.NumberTheory.Power
     ( squares
     , cubes
     , integralSqrt
     , isSquare
     , integralRoot
     ) where

import Data.Maybe (fromJust)
import Data.List (find)

{-| The square numbers, starting from 0.

    >>> take 5 squares
    [0, 1, 4, 9, 16]
-}
squares :: (Integral a) => [a]
squares = map (^2) [0..]

{-| The positive cubic numbers, starting from 0.

    >>> take 5 cubes
    [0, 1, 8, 27, 64]
-}
cubes :: (Integral a) => [a]
cubes = map (^3) [0..]

{-
   Generate potential roots using Newton's method.
   If x is an approximation for sqrt(n), then (x*x + n)/(2*x) is
   a better approximation.
-}
potentialRoots :: Integral a => a -> a -> [a]
potentialRoots k m =
  let -- To deal with fixed precision integers, we need to convert to
      -- Integer first.
      n = toInteger m
      l = toInteger k

      -- The initial approximation
      initial = 10^((toInteger $ length $ show n) `div` l)

      -- A list of improving approximations
      approximations = iterate initial
      iterate x = x:(iterate next)
        where next = x + dy `div` (l * x)
              dy = n - x^l

      -- Find the first approximation that does not result in
      -- a better approximation, call it potentialroot.
      differences = zipWith (\a b -> abs (a - b)) approximations (tail approximations)
      approxWithDiffs = zip approximations differences
      potentialRoot = fst $ fromJust $ find (\(a, b) -> b <= 1) approxWithDiffs

      -- The actual square root must be at most 1 off from the actual square
   in map fromIntegral
        [potentialRoot - 1, potentialRoot, potentialRoot + 1]

{-| @isSquare n@ returns whether @n@ is a perfect square.

    >>> isSquare 4
    True
    >>> isSquare 11
    False
    >>> isSquare 0
    True
-}
isSquare :: Integral a => a -> Bool
isSquare n
    | n < 0     = False
    | otherwise = any (== n) $ map (^2) $ potentialRoots 2 n

{-|
    @integralSqrt n@ computes the largest integer less than
    the square root of @n@.
-}
integralSqrt :: Integral a => a -> a
integralSqrt m =
  let -- To deal with fixed precision integers, we need to convert to
      -- Integer first.
      n = toInteger m
      pr = potentialRoots 2 n
      potentialRoots' = reverse $ [(head pr) - 1] ++ pr ++ [(last pr) + 1]
   in fromIntegral $ fromJust $ find (\x -> (x^2) <= n) potentialRoots'

{-|
    @integralRoot k n@ computes the largest integer less than
    the @k@th root of @n@.
-}
integralRoot :: (Integral a, Integral b) => b -> a -> a
integralRoot k n = fromIntegral $ search 0 n'
    where
      n' = toInteger n
      search :: Integer -> Integer -> Integer
      search lower upper
          | lower == upper = lower
          | otherwise      =
              case compare (midpoint^k) n' of
                  LT -> search midpoint upper
                  EQ -> midpoint
                  GT -> search lower (midpoint - 1)
                where
                  midpoint = (lower + upper + 1) `div` 2

