-- | Module      : Math.NumberTheory.Gaussian.Prime
--   Description : Functions related to Gaussian primes.
--   Copyright   : (c) Preetham Gujjula, 2024
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Functions related to Gaussian primes.
{-# LANGUAGE LambdaCase #-}
module Math.NumberTheory.Gaussian.Prime
  ( isPrime,
    primes,
  )
where

import Data.Complex (Complex ((:+)))
import Data.Bit (Bit (..))
import Data.List.ApplyMerge (applyMerge)
import Math.NumberTheory.Prime (isPrimeChimera)
import Data.Chimera (index)

applyMergeOn :: forall a b c d. Ord d => (c -> d) -> (a -> b -> c) -> [a] -> [b] -> [c]
applyMergeOn p f xs ys =
  let f' :: a -> b -> KeyValue d c
      f' x y =
        let z = f x y
         in KeyValue (p z) z
   in map (\(KeyValue _ z) -> z) (applyMerge f' xs ys)

norm :: Integral a => Complex a -> a
norm (x :+ y) = x*x + y*y

data KeyValue k v = KeyValue k v

instance (Eq k) => Eq (KeyValue k v) where
  (KeyValue k1 _) == (KeyValue k2 _) = k1 == k2
instance (Ord k) => Ord (KeyValue k v) where
  compare (KeyValue k1 _) (KeyValue k2 _) = compare k1 k2

isPrimeInteger :: Integral a => a -> Bool
isPrimeInteger n = n >= 0 && unBit (index isPrimeChimera (fromIntegral n))

-- | Whether a Gaussian integer is prime.
isPrime :: forall a. (Integral a) => Complex a -> Bool
isPrime =
  let is4k3 :: a -> Bool
      is4k3 m =
        if m > 0
        then (m `rem` 4) == 3
        else (m `rem` 4) == -1
   in \case
        a :+ 0 ->
          let a' = abs a
           in is4k3 a' && isPrimeInteger a'
        0 :+ b ->
          let b' = abs b
           in is4k3 b' && isPrimeInteger b'
        n -> isPrimeInteger (norm n)

-- | Enumerate the Gaussian integers, ordered by norm
enumerate :: (Integral a) => [Complex a]
enumerate =
  let zs = 0 : concatMap (\i -> [-i, i]) [1..]
   in applyMergeOn norm (:+) zs zs

-- | An infinite list of the Guassian primes. Primes are ordered by norm, then
-- by polar angle.
primes :: (Integral a) => [Complex a]
primes = filter isPrime enumerate
