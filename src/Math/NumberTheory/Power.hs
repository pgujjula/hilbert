{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module      : Math.NumberTheory.Power
--   Description : Functions related to powers.
--   Copyright   : (c) Preetham Gujjula, 2016
--   License     : GPL-3
--   Maintainer  : preetham.gujjula@gmail.com
--   Stability   : experimental
--
--   Functions related to powers.
module Math.NumberTheory.Power
  ( square,
    squares,
    cube,
    cubes,
    integralSqrt,
    isSquare,
    integralRoot,
    integralLogBase,
  )
where

import GHC.Exts (Int (..))
import GHC.Integer.Logarithms (integerLogBase#)

-- | Square a number. Useful to have, instead of writing x^2 and getting a
--   warning with -Wtype-defaults that the type of 2 is being inferred as
--   Integer.
square :: Num a => a -> a
square a = a * a

-- | The square numbers, starting from 0.
--
--   >>> take 5 squares
--   [0, 1, 4, 9, 16]
squares :: (Integral a) => [a]
squares = map square [0 ..]

-- | Cube a number. Useful to have, instead of writing x^3 and getting a
--   warning with -Wtype-defaults that the type of 3 is being inferred as
--   Integer.
cube :: (Integral a) => a -> a
cube a = a * a * a

-- | The positive cubic numbers, starting from 0.
--
--   >>> take 5 cubes
--   [0, 1, 8, 27, 64]
cubes :: (Integral a) => [a]
cubes = map cube [0 ..]

{-
   Generate potential roots using Newton's method.
   If x is an approximation for sqrt(n), then (x*x + n)/(2*x) is
   a better approximation.
-}

-- | @isSquare n@ returns whether @n@ is a perfect square.
--
--   >>> isSquare 4
--   True
--   >>> isSquare 11
--   False
--   >>> isSquare 0
--   True
isSquare :: Integral a => a -> Bool
isSquare n
  | n < 0 = False
  | otherwise = x * x == n
  where
    x = integralSqrt n

-- | @integralSqrt n@ computes the largest integer less than or equal to square
--   root of @n@.
integralSqrt :: forall a. Integral a => a -> a
integralSqrt n = fromIntegral $ search initial
  where
    -- To avoid overflow with fixed precision integers, we need to convert to
    -- Integer first.
    n' :: Integer
    n' = toInteger n

    -- The initial approximation
    initial :: Integer
    initial = maybe 2 (2 ^) (integralLogBase (2 :: Integer) n')

    search :: Integer -> Integer
    search x
      | dy == doublex = x
      | dx == 0 = x
      | otherwise = search (x + dx)
      where
        dy = n' - squarex
        dx = dy `div` doublex

        squarex = x * x
        doublex = 2 * x

-- | @integralRoot k n@ computes the largest integer less than the @k@th root of
--   @n@.
integralRoot :: (Integral a, Integral b) => b -> a -> a
integralRoot k n = fromIntegral $ search 0 n'
  where
    n' = toInteger n
    search :: Integer -> Integer -> Integer
    search lower upper
      | lower == upper = lower
      | otherwise =
          case compare (midpoint ^ k) n' of
            LT -> search midpoint upper
            EQ -> midpoint
            GT -> search lower (midpoint - 1)
      where
        midpoint = (lower + upper + 1) `div` 2

-- | @integralLogBase b n@ is the largest integer k such that b^k is less than or
--   equal to n.
--
--   >>> integralLogBase 2 10
--   3
integralLogBase :: (Integral b, Integral a) => b -> a -> Maybe Int
integralLogBase b n
  | b <= 1 = Nothing
  | n <= 0 = Nothing
  | otherwise = Just $ I# (integerLogBase# (toInteger b) (toInteger n))
