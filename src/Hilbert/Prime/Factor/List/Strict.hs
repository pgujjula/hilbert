{-|
    Module      : Hilbert.Prime.Factor.List.Strict
    Description : Factor the first n integers strictly.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Prime factorization.
-}
{-# LANGUAGE FlexibleContexts #-}
module Hilbert.Prime.Factor.List.Strict
  ( factorTo'
  ) where

import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray (assocs)
import Control.Monad (forM_)
import Hilbert.Prime.List.Strict (primesTo')
import Hilbert.Square (integralSqrt)

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


-- factorWith (n, factors) computes the prime factorization of n given
-- all of its prime factors, except for perhaps 1
factorWith :: (Int, [Int]) -> [(Int, Int)]
factorWith (n, []) = [(n, 1)]
factorWith (n, [x]) =
  let (e, n') = divideOut x n
   in if n' == 1
      then [(x, e)]
      else [(x, e), (n', 1)]
factorWith (n, (x:xs)) = 
  let (e, n') = divideOut x n
   in (x, e):(factorWith (n', xs))

{-|
    Compute the prime factorizations of every positive integer up to @n@. The
    underlying implementation performs the Sieve of Erastosthenes on an MArray,
    so it's fast but memory intensive.

    __Precondition:__ @n@ is nonnegative.

    >>> factorTo' 10
    [[],[(2,1)],[(3,1)],[(2,2)],[(5,1)],[(2,1),(3,1)],[(7,1)],[(2,3)],[(3,2)],[(2,1),(5,1)]]
-}
factorTo' :: Int -> [[(Int, Int)]]
factorTo' = ([]:)  . (map factorWith)
           . tail   . (map (\(a, b) -> (a, reverse b)))
           . assocs . primeFactors 

{-
   Get all strictly smaller prime factors of the integers up to n in array form.
   If k has prime factors [a, b, c], then array ! k == [a, b, c]. If p is a
   prime, then array ! p == []. Also, array ! 1 == []
-}
primeFactors n = runSTArray $ do
  array <- newArray (1, n) [] :: ST s (STArray s Int [Int])
  forM_ (primesTo' (integralSqrt n)) (\p -> insert array p n)
  return array

{-
   Insert p into the factor lists of all multiples of p in the array.
   The upper limit is passed in as a parameter.
-}
insert :: (MArray a [Int] m) => a Int [Int] -> Int -> Int -> m ()
insert array p limit = forM_ [2*p, 3*p..limit]
                          (\x -> do 
                            list <- readArray array x
                            writeArray array x (p:list))
