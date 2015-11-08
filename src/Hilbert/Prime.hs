module Hilbert.Prime (isPrime, primes, factor, factorizations) where

import Data.List (find, foldl', intercalate)
import Data.Maybe (fromJust)

import Hilbert.Prime.MillerRabin (millerRabin)
import Hilbert.Prime.Lucas (isPrimeLucas)
import Hilbert.Prime.BailliePSW (bailliePSW)

import Hilbert.Legendre (jacobi)
import Hilbert.Square (isSquare)
import Hilbert.Modular (modPow)
import Hilbert.Lucas
import System.Random
import Data.Ratio
import qualified Data.Map as Map

isPrime :: (Integral a) => a -> Bool
isPrime n
  | n <= 100 = isPrimeTrial n
  | otherwise = isPrime1 n

isPrimeTrial :: (Integral a) => a -> Bool
isPrimeTrial n | n <= 1 = False
isPrimeTrial n | n <= 3 = True
isPrimeTrial n = all (\a -> n `rem` a /= 0) testFactors
         where testFactors = 2:[3, 5..upperLimit]
               upperLimit = floor $ sqrt $ fromIntegral n

genvsch n = zip (map (\n -> ((u_seq 1 (-1)) !! n) `rem` 101) (path n))
                (map (\n -> ((v_seq 1 (-1)) !! n) `rem` 101) (path n))

primesUnder100 :: (Integral a) => [a]
primesUnder100 = [ 2,  3,  5,  7, 11,
                  13, 17, 19, 23, 29,
                  31, 37, 41, 43, 47,
                  53, 59, 61, 67, 71,
                  73, 79, 83, 89, 97]

--Precondition : n >= 100
isPrime1 :: (Integral a) => a -> Bool
isPrime1 n = (not $ any (\r -> n `rem` r == 0)
                       primesUnder100)
          && bailliePSW n

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

-- Factors the input into a list
factor :: (Integral a) => a -> [(a, a)]
factor n
  | n < 4     = [(n, 1)]
  | otherwise =
      if null afterFirstFactor
      then [(n, 1)]
      else include firstFactor rest
        where afterFirstFactor =
                snd $ break (`divides` n) potentialFactors
              firstFactor = head afterFirstFactor
              rest = factor (n `div` firstFactor)
              potentialFactors = [2..upperLimit]
              upperLimit = floor $ sqrt $ fromIntegral n
              include :: (Integral a) => a -> [(a, a)] -> [(a, a)]
              include x xs =
                let split = break (\(a, b) -> a >= x) xs
                    beginning = fst split
                    end = snd split
                in  if (not $ null end) && ((fst $ head end) == x)
                    then beginning ++ (fst $ head end, (snd $ (head end)) + 1):(tail end)
                    else beginning ++ ((x, 1):end)
              divides a b = (b `rem` a) == 0

factorizations :: Int -> [(Int, [(Int, Int)])]
factorizations limit = update prelim
  where
    prelim :: [(Int, [(Int, Int)])]
    prelim = foldl' merge blank (map genPowers validPrimes)
      where
        genPowers :: Int -> [(Int, (Int, Int))]
        genPowers n = zip [n, 2*n..limit] $ zip [n, n..] $ expos n
          where
            expos n = pattern n 1 []
              where pattern base n b4 = initial ++ (pattern base (n + 1) newseq)
                      where newseq = intercalate [n] $ cycleTimes base [b4]
                            initial = cycleTimes (base - 1) $ [n] ++ b4
                            cycleTimes 0 _ = []
                            cycleTimes n list = list ++ (cycleTimes (n - 1) list)
        merge :: [(Int, [(Int, Int)])] -> [(Int, (Int, Int))] -> [(Int, [(Int, Int)])]
        merge [] x = map (\(x1, x2) -> (x1, [x2])) x
        merge x [] = x
        merge (x:xs) (y:ys) =
          case compare (fst x) (fst y) of
            EQ -> (fst x, (snd y):(snd x)):(merge xs ys)
            GT -> (fst y, [snd y]):(merge (x:xs) ys)
            LT -> x:(merge xs (y:ys))
        validPrimes :: [Int]
        validPrimes = reverse
                    $ takeWhile (\n -> n <= (floor $ sqrt $ fromIntegral limit))
                                primes
        blank :: [(Int, [(Int, Int)])]
        blank = zip [1..limit] (repeat [])
    update = map check
     where
      check :: (Int, [(Int, Int)]) -> (Int, [(Int, Int)])
      check (a, b) = if newPrime > 1
                     then (a, (newPrime, 1):b)
                     else (a, b)
                      where newPrime = a `div` (toNum b)
      toNum :: [(Int, Int)] -> Int
      toNum = product . (map (\(a, b) -> a^b))
