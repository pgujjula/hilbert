{-|
    Module      : Hilbert.Prime.List.Erastosthenes.Strict
    Description : Strict Sieve of Erastosthenes
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Compute a list of prime numbers using the Sieve of Erastosthenes.
-}

{-# LANGUAGE FlexibleContexts #-}

module Hilbert.Prime.List.Erastosthenes.Strict
  ( primesTo'
  ) where

import Hilbert.Square (integralSqrt)

import Data.Array.ST
import Data.Array.IArray (assocs)
import Control.Monad.ST
import Control.Monad (forM_)

{-|
    @primesTo' n@ strictly computes the primes up to @n@, inclusive. Faster
    but higher memory usage than @primesTo n@.

    >>> primesTo' 10
    [2, 3, 5, 7]
-}
primesTo' :: Int -> [Int]
primesTo' n | n < 2 = []
primesTo' n = map fst $ filter snd $ assocs $ runSTUArray $ do
  array <- newArray (1, n) True :: ST s (STUArray s Int Bool)
  writeArray array 1 False
  sieveAll array 2 (integralSqrt n) n
  return array

{-
   Sieve all the primes between 'p' and 'limit' inclusive. The upper bound
   of the array is also passed in as 'n'.
-}
sieveAll :: (MArray a Bool m) => a Int Bool -> Int -> Int -> Int -> m ()
sieveAll array p limit n | p > limit = return ()
sieveAll array p limit n = do
  writeAllFalse array [2*p, 3*p..n]
  p <- findTrue array (p + 1)
  sieveAll array p limit n

-- Return the first true index starting from 'n'
findTrue :: (MArray a Bool m) => a Int Bool -> Int -> m Int
findTrue array n = do
  b <- readArray array n
  if b
  then return n
  else findTrue array (n + 1) 

-- Write False at all the given indices
writeAllFalse :: (MArray a Bool m) => a Int Bool -> [Int] -> m ()
writeAllFalse array xs = forM_ xs (\x -> writeArray array x False)
