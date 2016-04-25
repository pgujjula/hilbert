{-|
Module      : Hilbert.Digit
Description : Handle continued fractions.
Copyright   : (c) Preetham Gujjula, 2016
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental

Handle continued fractions. This module uses mathematics described in

    * Beceanu, Marius. "Period of the Continued Fraction of √n." Thesis.
      Princeton University, 2003. Web.
      <http://web.math.princeton.edu/mathlab/jr02fall/Periodicity/mariusjp.pdf>.
      Junior Thesis
    * Myerson, Gerry. "How to Detect When Continued Fractions Period
      Terminates." Mathematics Stack Exchange. Stack Exchange, 11 Dec. 2011.
      Web. 24 Apr. 2016. <http://math.stackexchange.com/a/90432>.

-}
module Hilbert.ContinuedFrac
  ( convergent
  , cfracSqrt) where

import Data.Maybe (fromJust)
import Data.List (find, findIndices)
import Hilbert.Square (isSquare, integralSqrt)
import Data.Ratio

{-
   Data type to represent quadratic surds.
   Surd a b c d represents (a sqrt(b) + c) / d.
-}
data Surd a = Surd a a a a
                deriving (Show, Eq)

{-
   Simplify a quadratic surd by removing common factors from a, c, and d. For
   example, simplify (Surd 12 3 15 18) == Surd 4 3 5 6.
-}
simplify :: (Integral a) => Surd a -> Surd a
simplify (Surd a b c d) = Surd a' b c' d'
  where [a', c', d'] = map (`div` g) [a, c, d]
        g = gcd a (gcd c d)

{-
   The reciprocal of a quadratic surd. We can show that the reciprocal of
   (a sqrt(b) + c) / d is equal to (ad sqrt(b) - cd) / (a^2 * b - c^2). We need
   to divide the terms a', c', d' by their gcd to keep all the terms relatively
   prime.
-}
reciprocal :: (Integral a) => Surd a -> Surd a
reciprocal (Surd a b c d) = simplify $ Surd a' b c' d'
  where a' = a*d
        c' = -c*d
        d' = (a^2)*b - (c^2)

{- Compute the floor of a quadratic surd. We have
    * f1 = floor (a sqrt(b))
          == integralSqrt (a^2 * b)
    * f2 = floor ((a sqrt(b) + c)
          == floor (a sqrt(b)) + c
          == f1 + c
    * answer = floor ((a sqrt(b)) + c) / d)
          == floor (floor(a sqrt(b) + c) / d)
          == floor (f2 / d)
          == f2 `div` d
        (see Beceanu, Lemma 2.1 for the proof of the last statement)
-}
surdFloor :: (Integral a) => Surd a -> a
surdFloor (Surd a b c d) =
  let f1 = integralSqrt (a^2 * b)
      f2 = f1 + c
   in f2 `div` d

-- Subtract an integer from a quadratic surd.
minus :: (Integral a) => Surd a -> a -> Surd a
minus (Surd a b c d) n = simplify $ Surd a b (c - d*n) d

{-|
   @convergent xs@ computes the rational number equal to the continued
   fraction represented by @xs@.

   For example, to compute convergents of /e/, whose continued fraction
   expansion is @[2, 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1...]@

   >>> let cfrac = (2:) $ concat $ map (\x -> [1, x, 1]) [2, 4..]
   >>> fromRational $ convergent (take 21 cfrac)
   2.718281828459045
   >>> exp 1
   2.718281828459045
-}
convergent :: (Integral a) => [a] -> Ratio a
convergent []     = 0
convergent [x]    = fromIntegral x
convergent (x:xs) = (fromIntegral x) + (1/(convergent xs))

{-
   Generate the continued fraction expansion of a (sqrt(b) + c) / d. Assumes
   that b is nonzero.
-}
cfracSurd :: (Integral a) => Surd a -> [a]
cfracSurd surd = k:(cfracSurd surd')
  where k = surdFloor surd
        surd' = reciprocal $ surd `minus` k

{-|
   @cfracSqrt x@ returns @'Nothing'@ if @x@ is a square. Otherwise, it returns
   @'Just' (n, xs)@, which represents the continued fraction representation of
   @sqrt x@.

    [@n@] is the non-repeating first term of the continued fraction,
    [@xs@] represents a single cycle of the repeating part of the continued
    fraction.

  >>> cfracSqrt 41
  Just (6, [2, 2, 12])
  >>> cfracSqrt 4
  Nothing

  For example, to compute a good approximation for @sqrt 2@,

  @
  approx = fromRational $ convergent (n:(repeat xs)) 30
           where Just (n, xs) = cfracSqrt 2
  @
  >>> approx - (sqrt 2)
  0.0
-}
cfracSqrt :: (Integral a) => a -> Maybe (a, [a])
cfracSqrt x | isSquare x = Nothing
cfracSqrt x = Just (first, periodicPart)
  where untruncated = cfracSurd (Surd 1 x 0 1)
        first = head untruncated

        -- According to Beceanu, Theorem 2.6, the last term, l, in the periodic
        -- part of the continued fraction of a surd, and the non-repeating term
        -- f, satisfy l == 2*f. According to Myerson, this is also a sufficient
        -- condition to determine l, so we can use it to find the periodic part
        -- of the continued fraction.
        periodicPart = takeUntil (== (2*first)) (tail untruncated)

        -- Like takeWhile, but includes the first failing value.
        takeUntil proc (x1:xs) = if (proc x1)
                                    then [x1]
                                    else x1:(takeUntil proc xs)
        takeUntil _ xs = xs
