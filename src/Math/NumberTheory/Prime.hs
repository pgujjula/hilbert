{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module      : Math.NumberTheory.Prime
--   Description : Functions related to primes.
--   Copyright   : (c) Preetham Gujjula, 2020
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Functions related to primes.
module Math.NumberTheory.Prime
  ( isPrime,
    primes,
    primesTo,
    primesFromTo,
    composites,
    compositesTo,
    primesChimera,
    isPrimeChimera,
  )
where

import Data.Bit (Bit, unsafeFlipBit)
import Control.Monad.ST (runST)
import Data.Chimera (Chimera, UChimera)
import Data.Chimera qualified as Chimera
import Data.Function ((&))
import Data.List.Ordered (minus)
import Data.Vector.Storable qualified as Storable
import Data.Vector.Unboxed.Mutable qualified as MUVector
import Data.Vector.Unboxed qualified as UVector
import Data.Word (Word64)
import Math.NumberTheory.Prime.Sieve (generateNPrimes, generatePrimes)
import Math.NumberTheory.Prime.Sieve qualified as PS
import Math.NumberTheory.Roots (integerSquareRoot)

-- | An infinite list of the primes.
--
--   >>> take 8 primes
--   [2, 3, 5, 7, 11, 13, 17, 19]
primes :: [Int]
primes = fmap fromIntegral PS.primes

-- | A list of the primes up to (and including) a limit.
--
--   >>> primesTo 10
--   [2, 3, 5, 7]
primesTo :: Int -> [Int]
primesTo n | n < 0 = []
primesTo n = fmap fromIntegral . PS.primesTo . fromIntegral $ n

-- | A list of the primes between a start and end, inclusive.
--
--   >>> primesFromTo 10 20
--   [11, 13, 17, 19]
primesFromTo :: Int -> Int -> [Int]
primesFromTo a b | a <= 0 = primesTo b
primesFromTo a b =
  PS.primesFromTo (fromIntegral a - 1) (fromIntegral b)
    & map fromIntegral

-- | Whether a number is prime.
--
--   >>> filter isPrime [1..10]
--   [2, 3, 5, 7]
isPrime :: (Integral a) => a -> Bool
isPrime n | n < 2 = False
isPrime n = isPrime' . fromIntegral $ n

isPrime' :: Word64 -> Bool
isPrime' n =
  not $
    any (\p -> n `rem` p == 0) $
      takeWhile (<= integerSquareRoot n) PS.primes

-- | A lazy infinite list of composities.
--
--   >>> takeWhile (<= 20) composites
--   [4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20]
composites :: [Int]
composites = [2 ..] `minus` primes

-- | A lazy infinite list of composities, up to a limit. Will be faster than
--   'composites'.
--
--   >>> compositesTo 20
--   [4, 6, 8, 9, 10, 12, 14, 15, 16, 18, 20]
compositesTo :: Int -> [Int]
compositesTo n = [2 .. n] `minus` primesTo n

-- | A @'Chimera'@ of primes.
primesChimera :: Chimera Storable.Vector Word64
primesChimera = Chimera.imapSubvectors f (Chimera.tabulate (const ()))
  where
    f :: Word -> UVector.Vector () -> Storable.Vector Word64
    f index vec =
      let start =
            if index == 0
              then 1
              else 1 + Chimera.index primesChimera (index - 1)
          count = fromIntegral (UVector.length vec)
       in generateNPrimes count start

-- | A 'Chimera' for 'isPrime'.
isPrimeChimera :: UChimera Bit
isPrimeChimera = Chimera.imapSubvectors f (Chimera.tabulate (const ()))
  where
    f :: Word -> UVector.Vector () -> UVector.Vector Bit
    f index vec =
      let lower = fromIntegral index
          upper = fromIntegral index + fromIntegral (UVector.length vec) - 1
          ps :: Storable.Vector Word64
          ps = generatePrimes lower upper
       in runST $ do
            isPrimeVec <- MUVector.new (UVector.length vec)
            Storable.forM_ ps $ \p ->
              unsafeFlipBit isPrimeVec (fromIntegral p - fromIntegral index)
            UVector.unsafeFreeze isPrimeVec
