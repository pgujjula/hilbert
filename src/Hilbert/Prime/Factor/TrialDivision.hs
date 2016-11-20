{-|
    Module      : Hilbert.Prime.Factor.TrialDivision
    Description : Factor an integer using trial division.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Factor an integer using trial division.
-}

module Hilbert.Prime.Factor.TrialDivision
  ( factor
  ) where

import Hilbert.Square (integralSqrt)
import Hilbert.Prime.List.Erastosthenes.Lazy (primes)

{-|
    Factor an integer using trial division.

    __Precondition:__ Input is nonzero.

    >>> factor 60
    [(2, 2), (3, 1), (5, 1)]
    >>> factor 1
    []
    >>> factor (-10)
    [(-1, 1), (2, 1), (5, 1)]
-}
factor :: (Integral a) => a -> [(a, a)]
factor x | x < 0 = (-1, 1):(factor (-x))
factor 0 = error "Can't factor 0"
factor x = factorFromList x (takeWhile (<= (integralSqrt x)) (map fromIntegral primes))

{-
   divideOut k n = (e, n'), where
     * k^e divides n, but k^(e + 1) does not (i.e., e = k || n)
     * n' = n / (k^e)
-}
divideOut :: (Integral a) => a -> a -> (a, a)
divideOut k n = if n `rem` k == 0
                then let (f, n') = divideOut k (n `quot` k)
                     in (f + 1, n')
                else (0, n)

{-
   divideFirst n list finds the first factor 'k' of 'n' in 'list'. If
   k || n = e, and n / (k ^ e) = n', then this function returns
   Just ((k, e), n', <elements after k>)
-}
divideFirst :: (Integral a) => a -> [a] -> Maybe ((a, a), a, [a])
divideFirst n [] = Nothing
divideFirst n (k:ks) =
  let (e, n') = divideOut k n
   in if e > 0
      then Just ((k, e), n', ks)
      else divideFirst n ks

{-
   factorFromList n list computes the prime factorization of n using the
   primes in list.
-}
factorFromList :: (Integral a) => a -> [a] -> [(a, a)]
factorFromList 1 _ = []
factorFromList n list =
  case divideFirst n list of
    Nothing -> [(n, 1)]
    Just ((k, e), n', ks) -> 
      (k, e):(factorFromList n' (takeWhile (<= (integralSqrt n')) ks))
