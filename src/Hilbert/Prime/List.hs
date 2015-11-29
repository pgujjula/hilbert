module Hilbert.Prime.List (primes) where

import Data.Map as Map

primes :: (Integral a) => [a]
primes = sieve [2..]

sieve :: (Integral a) => [a] -> [a]
sieve xs = sieve' xs Map.empty

sieve' :: (Integral a) => [a] -> Map.Map a [a] -> [a]
sieve' []     table = []
sieve' (x:xs) table =
  case Map.lookup x table of
    -- x is a prime, so insert (x*x, [x]) into the table.
    Nothing      -> x : (sieve' xs newtable)
                      where newtable = Map.insert (x*x) [x] table

    -- x is not a prime, and we are returned a list of primes that
    -- have x as the next number to cross out
    Just factors -> sieve' xs (reinsert tableWithoutX factors)
                      where reinsert table [] = table
                            reinsert table (prime:rest) = reinsert newTable rest
                                                          where newTable = (Map.insertWith (++) (x+prime) [prime] table)
                            tableWithoutX = Map.delete x table
