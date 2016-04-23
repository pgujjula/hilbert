{-|
Module      : Hilbert.Digit
Description : Functions to handle continued fractions.
Copyright   : (c) Preetham Gujjula, 2016
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental

Functions to handle continued fractions.
-}
module Hilbert.ContinuedFrac
  ( convergent
  , cfracSqrt) where

import Data.Maybe (fromJust)
import Data.List (find, findIndices)
import Hilbert.Square (isSquare, integralSqrt)
import Data.Ratio

-- Data type to represent quadratic surds
-- Irrational a b c d corresponds to (a sqrt(b) + c)/d
data Irrational a = Irrational a a a a
                deriving (Show, Eq)

-- Take the reciprocal of a quadritic surd
reciprocal :: (Integral a) => Irrational a -> Irrational a
reciprocal (Irrational a b c d) =
          Irrational
              (newa `div` g)
              b
              (newc `div` g)
              (newd `div` g)
  where g = gcd (gcd newa newc) newd
        newa = a*d
        newc = -c*d
        newd = (a^2)*b - (c^2)

-- Compute the floor of a quadratic surd
flr :: (Integral a) => Irrational a -> a
flr (Irrational a b c d) = floor $ ((f a)*sqrt(f b) + (f c)) / (f d)
  where f = fromIntegral

-- Subtract an integer from a quadratic surd
minus :: (Integral a) => Irrational a -> a -> Irrational a
minus (Irrational a b c d) n = Irrational (newa `div` g)
                                          b
                                          (newc `div` g)
                                          (newd `div` g)
  where newa = a
        newc = (c - d*n)
        newd = d
        g = gcd (gcd newa newc) newd

{- |@convergent xs@ computes the rational number equal to the continued
   fraction represented by @xs@.
   
   For example, to compute convergents of /e/, whose continued fraction
   expansion is @[2, 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1...]@
   
   >>> let cfrac = (2:) $ concat $ map (\x -> [1, x, 1]) [2, 4..]
   >>> convergent (take 10 cfrac)
   1457 % 536
   >>> (fromRational $ convergent (take 21 cfrac)) == (exp 1)
   True
-}
convergent :: (Integral a) => [a] -> Ratio a
convergent []     = 0
convergent [x]    = fromIntegral x
convergent (x:xs) = (fromIntegral x) + (1/(convergent xs))

{- |@cfracSqrt x@ returns @'Nothing'@ if @x@ is a square. Otherwise, it
  returns @'Just' (n, xs)@, which represents the continued fraction representation
  of @sqrt x@.

    [@n@] is the non-repeating first term of the continued fraction,
    [@xs@] represents a single cycle of the repeating part of the continued fraction.

  KNOWN ISSUE: Does not work with integers larger than about 100, due to floating point 
  errors. Will be fixed soon.

  >>> cfracSqrt 41
  Just (6, [2, 2, 12])
  >>> cfracSqrt 4
  Nothing

  For example, to compute a good approximation for @sqrt 2@

  @
  approx = fromRational $ convergent (n:(repeat xs)) 30
           where Just (n, xs) = cfracSqrt 2
  @
  >>> approx - (sqrt 2)
  0.0
-}
cfracSqrt :: (Integral a) => a -> Maybe (a, [a])
cfracSqrt x | isSquare x = Nothing
cfracSqrt x = Just $ (first, periodic)
  where untruncated = cfracSqrtUntruncated x
        first = head untruncated
        rest = tail untruncated
        periodic = takeUntil (== (2*first)) rest

        takeUntil proc (x1:xs) = if (proc x1)
                                    then [x1]
                                    else x1:(takeUntil proc xs)
        takeUntil _ xs = xs
        cfracSqrtUntruncated x = map fst $ generate (a0, b0)
          where a0 = integralSqrt x
                b0 = reciprocal (Irrational 1 x (-a0) 1)
                generate (a, b) = (a, b):(generate (flr b, reciprocal(b `minus` flr(b))))
