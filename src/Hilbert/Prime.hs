module Hilbert.Prime (isPrime, primes, factor, factorizations) where

import Data.List (find, foldl', intercalate)
import Data.Maybe (fromJust)
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

condition2 n = any (== 0) $ take (fromIntegral s)
             $ tail
             $ reverse
             $ map snd
             $ collapseMap' p q (1, p) (path del) (n)
               where p = 1
                     q = (1 - d) `div` 4
                     d = findd n
                     (o, s) = factorOut del
                     del = n - (jacobi d n)

condition2' n =collapseMap' p q (1, p) (path del) (n)
               where p = 1
                     q = (1 - d) `div` 4
                     d = findd n
                     (o, s) = factorOut del
                     del = n - (jacobi d n)

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
                   

-- Baillie-PSW primality test
-- Precondition: n >= 3
bailliePSW :: (Integral a) => a -> Bool
bailliePSW n = (millerRabin n [2]) && (isPrimeLucas n)

-- Miller-Rabin probabilistic prime test
-- Precondition: n >= 3
millerRabin :: (Integral a) => a -> [a] -> Bool
millerRabin n testBases = not $ all (baseTest n) testBases
  where baseTest n a = (part1 n a) && (part2 n a)
        part1 n a = (modPow a d n) /= 1
        part2 n a = all (\r -> (modPow a (2^r * d) n) /= (n - 1))
                        [0..s - 1]
        d = (n - 1) `div` (2^(numTwos (n - 1)))
        s = numTwos (n - 1)
        numTwos n = if odd n
                    then 0
                    else 1 + (numTwos (n `div` 2))

-- Lucas probabilistic prime test
-- precondition :: n >= 3
isPrimeLucas :: (Integral a) => a -> Bool
isPrimeLucas n = n == 2 || precondition && condition
   where d = findd n
         p = 1
         q = (1 - d) `div` 4
         (o, e) = factorOut del
         del = n - (jacobi d n)
         precondition = n `rem` 2 == 1
                     && not (isSquare n)
                     && gcd n d == 1
                     && gcd n q == 1
         condition = condition1 n
                  || condition2 n

condition1 n = 
  let d = findd n
      p = 1
      q = (1 - d) `div` 4
      (o, e) = factorOut del
      del = n - (jacobi d n)
   in lucasu p q o n == 0

findd :: (Integral a) => a -> a
findd n = fromJust $ find (\a -> jacobi a n == -1) potentiald

potentiald :: (Integral a) => [a]
potentiald = alternate [5, 9..] [-7, -11..]

alternate :: [a] -> [a] -> [a]
alternate (x:xs) (y:ys) = x:y:(alternate xs ys)

factorOut :: (Integral a) => a -> (a, a)
factorOut n = if odd n
              then (n, 0)
              else (olda, oldb + 1) 
                  where (olda, oldb) = factorOut (n `div` 2)

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

