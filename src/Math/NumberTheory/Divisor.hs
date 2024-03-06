{-# LANGUAGE ImportQualifiedPost #-}

-- | Module      : Math.NumberTheory.Divisor
--   Description : Divisors of integers.
--   Copyright   : (c) Preetham Gujjula, 2020
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Divisors of integers.
module Math.NumberTheory.Divisor
  ( -- * Basic functions
    divides,
    relativelyPrime,

    -- * Listing divisors
    divisors,
    divisorsF,
    divisorPairs,
    divisorPairsF,

    -- * Summarizing divisors
    numDivisors,
    numDivisorsF,
    sumDivisors,
    sumDivisorsF,

    -- * Totient function
    totient,
    totientF,

    -- * Mobius function
    mobius,
    mobiusF,
    mobiuses,
    mobiusesFrom,
  )
where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Function ((&))
import Data.List (foldl')
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector
import Math.NumberTheory.Power (square)
import Math.NumberTheory.Prime.Factor (Factorization, factor)
import Math.NumberTheory.Prime.SegmentedSieve
  ( Chunk,
    largestMultipleLE,
    sieveAllChunks,
    smallestMultipleGE,
  )

-- | @a `divides` b@ is True iff there is an integer @k@ such that @a*k == b@.
--
--   >>> 5 `divides` 30
--   True
--   >>> 5 `divides` 31
--   False
--   >>> 0 `divides` 3
--   False
--   >>> 0 `divides` 0
--   True
divides :: (Integral a) => a -> a -> Bool
divides a b
  | a == 0 = b == 0
  | otherwise = b `rem` a == 0

-- | @relativelyPrime m n@ is @True@ if @m@ and @n@ have no common positive
--   divisors other than 1.
--
--   >>> relativelyPrime 3 5
--   True
--   >>> relativelyPrime 4 6
--   False
--   >>> relativelyPrime 0 1
--   True
--   >>> relativelyPrime 0 0
--   False
relativelyPrime :: (Integral a) => a -> a -> Bool
relativelyPrime m n = gcd m n == 1

-- | The positive divisors of an integer. Not necessarily in sorted order.
--
--   >>> divisors 6
--   [1, 3, 2, 6]
--   >>> divisors (-4)
--   [1, 2, 4]
--   >>> divisors 0
--   []
divisors :: (Integral a) => a -> [a]
divisors n =
  if n == 0
    then []
    else divisorsF . factor . abs $ n

-- | The positive divisors of an integer, from its factorization. Not necessarily
--   in sorted order.
--
--   >>> prime = unsafeMarkPrime
--   >>> divisorsF [(prime 2, 1), (prime 3, 1)]
--   [1, 3, 2, 6]
--   >>> divisors []
--   [1]
divisorsF :: (Integral a) => Factorization a -> [a]
divisorsF = foldl' (liftA2 (*)) [1] . map pows
  where
    pows (p, e) = take (e + 1) $ iterate (* p) 1

-- | The positive divisors of an integer @n@, paired up such that every pair
--   @(a, b)@ satisfies @a*b == abs n@. Not in any particular order.
--
--   >>> divisorPairs 6
--   [(1, 6), (3, 2)]
--   >>> divisorPairs (-9)
--   [(1, 9), (3, 3)]
divisorPairs :: (Integral a) => a -> [(a, a)]
divisorPairs = mkPairs . divisors

-- | The positive divisors of an integer @n@, from its prime factorization,
--   paired up such that every pair @(a, b)@ satisfies @a*b == abs n@. Not in any
--   particular order.
--
--   >>> prime = unsafeMarkPrime
--   >>> divisorPairsF [(prime 2, 1), (prime 3, 1)]
--   [(1, 6), (3, 2)]
--   >>> divisorPairsF []
--   [(1, 1)]
divisorPairsF :: (Integral a) => Factorization a -> [(a, a)]
divisorPairsF = mkPairs . divisorsF

mkPairs :: [a] -> [(a, a)]
mkPairs xs = take ((length xs + 1) `quot` 2) $ zip xs (reverse xs)

-- | The number of positive divisors of @n@.
--
--   >>> numDivisors 12
--   6
--   >>> numDivisors 1
--   1
--   >>> numDivisors 0
--   0
numDivisors :: (Integral a) => a -> a
numDivisors n =
  if n == 0
  then 0
  else numDivisorsF . factor . abs $ n

-- | The number of positive divisors of @n@, from its factorization.
--
--   >>> prime = unsafeMarkPrime
--   >>> numDivisorsF [(prime 2, 2), (prime 3, 1)]
--   6
--   >>> numDivisorsF []
--   1
numDivisorsF :: (Integral a) => Factorization a -> a
numDivisorsF = foldl' (*) 1 . map (\(_, e) -> fromIntegral e + 1)

-- | The sum of the positive divisors of @n@.
--
--   >>> sumDivisors 12
--   28
--   >>> sumDivisors 1
--   1
--   >>> sumDivisors 0
--   0
sumDivisors :: (Integral a) => a -> a
sumDivisors n =
  if n == 0
  then 0
  else sumDivisorsF . factor . abs $ n

-- | The sum of the positive divisors of @n@, from its factorization.
--
--   >>> prime = unsafeMarkPrime
--   >>> sumDivisorsF [(prime 2, 2), (prime 3, 1)]
--   28
--   >>> sumDivisorsF []
--   1
sumDivisorsF :: (Integral a) => Factorization a -> a
sumDivisorsF = foldl' (*) 1 . map f
  where
    f (p, e) = (p ^ (e + 1) - 1) `quot` (p - 1)

-- | @totient n@ is the number of integers in @[1..n]@ that are relatively prime
--   to @n@. Also, @totient 0 == 1@ by convention.
--
--   >>> totient 10
--   4
--   >>> totient (-4)
--   0
totient :: (Integral a) => a -> a
totient n
  | n < 0 = 0
  | n == 0 = 1
  | otherwise = totientF n (factor n)

-- | Like 'totient', but takes the factorization of @n@ to speed up the
--   computation of the totient.
--
--   >>> prime = unsafeMarkPrime
--   >>> totient 10 [(prime 2, 1), (prime 5, 1)]
--   4
totientF :: (Integral a) => a -> Factorization a -> a
totientF = foldl' step
  where
    step n (p, _) = n * (p - 1) `quot` p

-- | @mobius n@ is the Möbius function. It is defined as
--   * 0 if n is divisible by a square (that is not 1).
--   * 1 if n is square-free and has an even number of prime factors.
--   * -1 if n is square-free and has an odd number of prime factors.
--
--   It is not defined for @n <= 0@.
mobius :: (Integral a) => a -> a
mobius n =
  if n <= 0
    then error "mobius: non-positive input"
    else mobiusF . factor $ n

-- | Like 'mobius', but takes the factorization for faster computation.
mobiusF :: (Integral a) => Factorization a -> a
mobiusF fact
  | any ((> 1) . snd) fact = 0
  | even (length fact) = 1
  | otherwise = -1

-- | The mobius function on the postive integers
--
--   >>> take 10 mobiuses
--   [1,-1,-1,0,-1,1,-1,0,0,1]
mobiuses :: [Int]
mobiuses = mobiusesFrom 1

-- | The mobius function starting from @n@.
--
--   >>> take 10 (mobiusesFrom 5)
--   [-1,1,-1,0,0,1,-1,0,-1,1]
mobiusesFrom :: Int -> [Int]
mobiusesFrom start =
  sieveAllChunks start mobiusSieve
    & concatMap Vector.toList
    & zipWith mkMobius [start ..]
  where
    mkMobius :: Int -> Int -> Int
    mkMobius n signedProd
      | signedProd == 0 = 0
      | abs signedProd == n = signum signedProd
      | otherwise = -signum signedProd

    mobiusSieve :: [Int] -> Chunk -> Vector Int
    mobiusSieve ps (_, n, m) = runST $ do
      v <- MVector.replicate (m - n + 1) 1
      forM_ ps $ \p -> do
        let lower = smallestMultipleGE p n
        let upper = largestMultipleLE p m
        forM_ [lower, lower + p .. upper] $ \i ->
          MVector.unsafeModify v ((-p) *) (i - n)

        forM_ (takeWhile (<= m) $ iterate (* p) (square p)) $ \pk ->
          let lower' = smallestMultipleGE pk n
              upper' = largestMultipleLE pk m
           in forM_ [lower', lower' + pk .. upper'] $ \i -> do
                MVector.unsafeWrite v (i - n) 0

      Vector.unsafeFreeze v
