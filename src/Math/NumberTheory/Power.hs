{-# LANGUAGE MagicHash #-}

-- | Module      : Math.NumberTheory.Power
--   Description : Functions related to powers.
--   Copyright   : (c) Preetham Gujjula, 2016
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Functions related to powers.
module Math.NumberTheory.Power
  ( square,
    squares,
    cube,
    cubes,
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
