{-|
    Module      : Hilbert.Prime.Factor.List.Lazy
    Description : Factor the positive integers lazily
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Factor the positive integers lazily
-}

module Hilbert.Prime.Factor.List.Lazy
  ( factorTo
  , factorToInf
  ) where

import Control.Monad.State.Lazy
import Control.Applicative ((<$>))
import Data.List (mapAccumL)
import Data.Tuple (swap)

import Hilbert.PriorityQueue
import Hilbert.Prime.List.Lazy (primes)
import Hilbert.Square (integralSqrt)

type PQueue = DefaultQueue

{-| 
    @factorTo 10@ factors the first @n@ positive integers lazily. Lower memory
    usage and but slower than @'factorTo''@.

    __Precondition:__ @n@ is nonnegative

    >>> factorTo 10
    [[],[(2,1)],[(3,1)],[(2,2)],[(5,1)],[(2,1),(3,1)],[(7,1)],[(2,3)],[(3,2)],[(2,1),(5,1)]]
-}
factorTo :: Int -> [[(Int, Int)]]
factorTo n = (prefix ++) $ snd $ mapAccumL step' startQueue [3..n]
  where step' a b = swap (step b a)
        prefix = [[], [(2, 1)]]
        startQueue = insert 2 4 empty
        limit = integralSqrt n

        step :: Int -> PQueue Int Int -> ([(Int, Int)], PQueue Int Int)
        step n = runState $ do
            primality <- (n <) <$> (gets (snd . peekMinP))
            if primality
            then do
              if n <= limit
              then modify (insert n (n^2))
              else return ()
              return [(n, 1)]
            else do
              removedPrimes <- state deleteAllMin
              forM_ removedPrimes $ \p ->
                modify (insert p (n + p))
              return (factorUsing removedPrimes n)

{-|
    Get a lazy infinite list of factorizations of the positive integers.
    Equivalent to @map factor [1..]@. Keep in mind @take n factorToInf@ is
    likely slower than either @factorTo n@ or @factorTo' n@.

    >>> take 10 factorToInf
    [[],[(2,1)],[(3,1)],[(2,2)],[(5,1)],[(2,1),(3,1)],[(7,1)],[(2,3)],[(3,2)],[(2,1),(5,1)]]
-}
factorToInf :: [[(Int, Int)]]
factorToInf = (prefix ++) $ snd $ mapAccumL step' startQueue [3..]
  where step' a b = swap (step b a)
        prefix = [[], [(2, 1)]]
        startQueue = insert 2 4 empty

        step :: Int -> PQueue Int Int -> ([(Int, Int)], PQueue Int Int)
        step n = runState $ do
            primality <- (n <) <$> (gets (snd . peekMinP))
            if primality
            then do
              modify (insert n (n^2))
              return [(n, 1)]
            else do
              removedPrimes <- state deleteAllMin
              forM_ removedPrimes $ \p ->
                modify (insert p (n + p))
              return (factorUsing removedPrimes n)

factorUsing :: [Int] -> Int -> [(Int, Int)]
factorUsing factors n = (zip factors exponents) ++ extra
  where (leftover, exponents) = mapAccumL divideOut n factors
        extra = if leftover == 1
                then []
                else [(leftover, 1)]

{-
   divideOut n, k = (n', e), where
     * k^e divides n, but k^(e + 1) does not (i.e., e = k || n)
     * n' = n / (k^e)
-}
divideOut :: Int -> Int -> (Int, Int)
divideOut n k = if n `rem` k == 0
                then let (n', f) = divideOut (n `quot` k) k
                     in (n', f + 1)
                else (n, 0)
