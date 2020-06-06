{-| Module      : Math.NumberTheory.Prime.Factor
    Description : Prime factorizationa.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Prime factorization.
-}

module Math.NumberTheory.Prime.Factor
    ( Factorization
    , multiply
    , pow
    , simplify
    , factor
    , factorizations
--    , factorizationsTo
    ) where

import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap
import           Data.List               (foldl')
import           Data.Maybe              (fromMaybe)
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector

import           Data.Chimera            (Chimera)
import qualified Data.Chimera            as Chimera

import           Data.List.Duplicate     (groupAdj)

import           Math.NumberTheory.Power (integralSqrt)
import           Math.NumberTheory.Prime (primes, Prime, unPrime, unsafeMarkPrime)

type Factorization a = [(Prime a, Int)]

multiply :: (Integral a) => Factorization a -> Factorization a -> Factorization a
multiply xs [] = xs
multiply [] ys = ys
multiply ((px, ex):xs) ((py, ey):ys) =
    case compare px py of
        LT -> (px, ex)      : multiply xs ((py, ey):ys)
        EQ -> (px, ex + ey) : multiply xs ys
        GT -> (py, ey)      : multiply ((px, ex):xs) ys

pow :: Factorization a -> Int -> Factorization a
pow fs k = map (\(p, e) -> (p, e*k)) fs

simplify :: (Integral a) => Factorization a -> a
simplify = product . map (\(p, e) -> unPrime p ^ e)

count :: (Ord a) => [a] -> [(a, Int)]
count = map (\xs -> (head xs, length xs)) . groupAdj

factor :: (Integral a) => a -> Maybe (Factorization a)
factor n
    | n <= 0    = Nothing
    | n == 1    = Just []
    | otherwise = Just
                $ count
                $ factorWith (map (fmap fromIntegral) primes) (integralSqrt n) n

factorWith :: (Integral a) => [Prime a] -> a -> a -> [Prime a]
factorWith (p:ps) limit n
    | unP > limit = [unsafeMarkPrime n]
    | r == 0      = p : factorWith (p:ps) (integralSqrt n') n'
    | otherwise   = factorWith ps limit n
  where
    (n', r) = n `quotRem` unP
    unP = unPrime p
factorWith _ _ _ = error "ran out of primes, this is impossible"

-- smallestFactor !! i is the smallest prime factor of i + 2.
smallestFactor :: [Int]
smallestFactor = 2 : sieve (IntMap.singleton 4 [2]) [3..]
  where
    sieve mp (x:xs) =
        case IntMap.minViewWithKey mp of
            Nothing -> []
            Just ((n, ps), mp') ->
                case compare n x of
                    -- The first number in the map was too small. This should not happen
                    LT -> error "missing numbers from input stream"

                    -- Not a prime, and we have a list of witnesses ps to prove it.
                    -- Reinsert the primes at their next multiples.
                    EQ -> minimum ps : sieve (reinsert (n, ps) mp') xs

                    -- found a prime. yield it and insert it into the list at the first
                    -- multiple that matters: x^2
                    GT -> x : sieve (insert (x*x, x) mp) xs
    sieve _ [] = error "sieving empty list"

    -- Given a number and its prime divisors, generate a list of the next
    -- multiple for each prime, so we can reinsert these into the map.
    updates :: (Int, [Int]) -> [(Int, Int)]
    updates (n, ps) = map (\p -> (n + p, p)) ps

    -- Insert a (number, prime divisor) pair into a map
    insert :: (Int, Int) -> IntMap [Int] -> IntMap [Int]
    insert (n, p) = IntMap.alter (pushPrime p) n
      where
        pushPrime q xs = Just (q : fromMaybe [] xs)

    -- After popping a number and its prime divisors, reinsert each prime back
    -- into the map at its next multiple.
    reinsert :: (Int, [Int]) -> IntMap [Int] -> IntMap [Int]
    reinsert (d, ps) mp = foldl' (flip insert) mp (updates (d, ps))

smallestFactorC :: Chimera Vector Word
smallestFactorC = Chimera.mapSubvectors (Vector.map (fromIntegral . head)) $ Chimera.iterate tail (0:0:smallestFactor)

factorizations' :: Chimera Vector [Word]
factorizations' = Chimera.tabulateFix factorF
  where
    factorF :: (Word -> [Word]) -> Word -> [Word]
    factorF f n
        | n <= 1    = []
        | otherwise = p : f (n `quot` p)
      where
        p = Chimera.index smallestFactorC n

factorizations :: [Factorization Int]
factorizations = map count $ tail
               $ map (map (unsafeMarkPrime . fromIntegral))
               $ Chimera.toList factorizations'

--factorizationsTo :: Int -> [Factorization Int]
--factorizationsTo = undefined
