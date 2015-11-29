module Hilbert.Prime.Factor (factor, factorizations) where

import Data.List (intercalate, foldl')

import Hilbert.Prime.List (primes)

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
