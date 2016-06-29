{-|
    Module      : Hilbert.Prime.List
    Description : An infinite list of prime numbers.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    An infinite list of prime numbers. This module uses mathematics described in
    
        * Neill, Melissa O. "The Genuine Sieve of Eratosthenes." Journal of
          Functional Programming 19.1 (2009): 95-106. Cambridge University
          Press. Web.
-}

module Hilbert.Prime.List.Lazy
  ( primes
  , primesTo
  ) where

import Control.Monad.State.Lazy
import Control.Applicative ((<$>))
import Data.Tuple (swap)
import Data.List (mapAccumL, findIndices)

import Hilbert.PriorityQueue
import Hilbert.Square (integralSqrt)

type PQueue = DefaultQueue

{-|
   A lazy, infinite list of primes. The algorithm used is described in
    
        * Neill, Melissa O. "The Genuine Sieve of Eratosthenes." Journal of
          Functional Programming 19.1 (2009): 95-106. Cambridge University
          Press. Web.

   >>> take 10 primes
   [2, 3, 5, 7, 11, 13, 17, 19, 23]
-}
primes :: [Int]
primes = findIndices id
       $ ([False, False] ++)
       $ snd
       $ mapAccumL step' (insert 2 4 empty) [2..]
  where step' a b = swap (step b a)

        step :: Int -> PQueue Int Int -> (Bool, PQueue Int Int)
        step n = runState $ do
          primality <- (n <) <$> (gets (snd . peekMinP))
          if primality
          then modify (insert n (n^2))
          else (state deleteAllMin) >>= mapM_ (\p -> modify (insert p (n + p)))
          return primality

primesTo :: Int -> [Int]
primesTo n = findIndices id
        $ ([False, False] ++)
        $ snd
        $ mapAccumL step' (insert 2 4 empty) [2..n]

  where step' a b = swap (step b a)
        limit = integralSqrt n

        step :: Int -> PQueue Int Int -> (Bool, PQueue Int Int)
        step n = runState $ do
          primality <- (n <) <$> (gets (snd . peekMinP))
          if primality
          then if n <= limit
               then modify (insert n (n^2))
               else return ()
          else (state deleteAllMin) >>= mapM_ (\p -> modify (insert p (n + p)))
          return primality
