{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | Module      : Math.Combinatorics.Binomial
--   Description : Functions related to the binomial coefficient.
--   Copyright   : (c) Preetham Gujjula, 2020
--   License     : BSD-3-Clause
--   Maintainer  : preetham.gujjula@gmail.com
--   Stability   : experimental
--
--   Functions related to the binomial coefficient.
module Math.Combinatorics.Binomial
  ( factorial,
    choose,
    binomialCoeffs,
    pascalDiagonal,
    permute,

    -- * Binomial coefficients \(\text{mod } p\)
    FactorialModP,
    mkFactorialModP,
    factorialModP,
    factorialRelPrimeModP,
    factorialNoPModP,
    chooseModP,

    -- * Binomial coefficients \(\text{mod } p^2\)
    FactorialModP2,
    mkFactorialModP2,
    factorialRelPrimeModP2,
    factorialNoPModP2,
    chooseModP2,
  )
where

import Data.Euclidean (Euclidean, quot)
import Data.Function ((&))
import Data.List (foldl', scanl')
import Data.Mod (Mod)
import Data.Semiring (Semiring, fromNatural, one, plus, product', times, zero)
import Data.Type.Natural (KnownNat, SNat, sNat, toNatural, type (^))
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import GHC.Real qualified as Integral (quot)
import Math.NumberTheory.Power (square)
import Numeric.Natural (Natural)
import Prelude hiding (quot)

-- | The factorial of a number. Undefined behavior for negative inputs.
--
--   >>> factorial 5
--   120
factorial :: (Integral a, Semiring b) => a -> b
factorial n | n <= 0 = one
factorial n = product' $ map (fromNatural . fromIntegral) [1 .. n]

-- | The binomial coefficient. @choose n k@ is defined as 0 for any @n < 0@ or
--   @k > n@ or @k < 0@.
--
--   >>> 5 `choose` 2
--   10
choose :: (Integral a, Euclidean b) => a -> a -> b
choose n k
  | n < 0 = zero
  | k > n = zero
  | k < 0 = zero
  | 2 * k > n = choose' n (n - k)
  | otherwise = choose' n k

-- no preconditions are checked
choose' :: forall a b. (Integral a, Euclidean b) => a -> a -> b
choose' n k =
  foldl' (\i (p, q) -> (i `times` p) `quot` q) one $
    zip nums denoms
  where
    nums :: [b]
    nums = map (fromNatural . fromIntegral) [n, n - 1 .. n - k + 1]

    denoms :: [b]
    denoms = map (fromNatural . fromIntegral) [1 .. k]

-- | Given n, yields the binomial coefficients of exponent n. More efficent than
--   mapping 'choose'. Undefined behavior if @n < 0@.
binomialCoeffs :: forall a b. (Integral a, Euclidean b) => a -> [b]
binomialCoeffs n | n < 0 = error "binomialCoeffs: negative input"
binomialCoeffs n = scanl' step one [0 .. n - 1]
  where
    step :: b -> a -> b
    step x k = (x `times` aToB (n - k)) `quot` aToB (k + 1)

    aToB :: a -> b
    aToB = fromNatural . fromIntegral

-- | The @n@th diagonal of Pascal's triangle. So,
--
-- > pascalDiagonal n == map (\m -> (n+m) `choose` m) [0..]
--
-- but more efficient
--
-- n `choose` 0    = n! / n! 0!      = 1
-- n+1 `choose` 1  = (n+1)! / n! 1!  = (n+1) `quot` 1
-- n+2 `choose` 2  = (n+2)! / n! 2!  = (n+2)*(n+1) `quot` 1*2
--                                   = (n+3)*(n+2)*(n+1) `quot` 1*2*3
pascalDiagonal :: forall a b. (Integral a, Euclidean b) => a -> [b]
pascalDiagonal n = scanl' step one [1 ..]
  where
    n' :: b
    n' = fromNatural . fromIntegral $ n

    step :: b -> Natural -> b
    step term m =
      let m' = fromNatural m
       in ((n' `plus` m') `times` term) `quot` m'

-- | Number of permutations groups of size @k@, selected from a group of size
--   @n@. @permute n k@ is defined as 0 for any @n < 0@ or @k > n@ or @k < 0@.
--
--   >>> 5 `permute` 2
--   20
permute :: (Integral a, Semiring b) => a -> a -> b
permute n k
  | n < 0 = zero
  | k > n = zero
  | k < 0 = zero
  | otherwise = product' $ map (fromNatural . fromIntegral) [n, n - 1 .. n - k + 1]

-- | A data structure that allows finding @('factorial' n) `'mod'` p@ quickly.
newtype FactorialModP p = FactorialModP {unFactorialModP :: Vector (Mod p)}
  deriving (Eq, Show, Ord)

class HasP a where
  getP :: a -> Integer

instance HasP (FactorialModP p) where
  getP = toInteger . Vector.length . unFactorialModP

-- | Generate a 'FactorialModP' in \(O(p)\) time.
mkFactorialModP :: forall p. KnownNat p => FactorialModP p
mkFactorialModP =
  let p :: Int
      p = fromIntegral . toNatural $ (sNat :: SNat p)
   in FactorialModP $ Vector.fromListN p $ scanl' (*) 1 [1 ..]

-- | Compute @('factorial' n) `'mod'` p@ in \(O(\log n)\) time.
factorialModP :: KnownNat p => FactorialModP p -> Integer -> Mod p
factorialModP (FactorialModP vec) n =
  let p = Vector.length vec
   in if
          | n < 0 -> error "factorialModP: negative argument"
          | n >= toInteger p -> 0
          | otherwise -> vec ! fromInteger n

-- | The product of the numbers less than @n@ relatively prime to @p@, modul
--   @p@.
factorialRelPrimeModP :: KnownNat p => FactorialModP p -> Integer -> Mod p
factorialRelPrimeModP (FactorialModP vec) n =
  if n < 0
    then error "factorialRelPrimeModP: negative argument"
    else
      let p = Vector.length vec
          (q, r) = n `quotRem` toInteger p
       in (-1) ^ q * (vec ! fromInteger r)

-- | The factorial of @n@, without the factors of @p@, modulo @p@.
factorialNoPModP :: KnownNat p => FactorialModP p -> Integer -> Mod p
factorialNoPModP fmp n =
  let p = toInteger (Vector.length (unFactorialModP fmp))
   in iterate (`quot` p) n
        & takeWhile (> 0)
        & map (factorialRelPrimeModP fmp)
        & product

-- | Efficient computation of snd (divideOut (factorial n) p)
divideOutFactorialP :: Integral a => a -> a -> a
divideOutFactorialP n p =
  iterate (`Integral.quot` p) n
    & tail
    & takeWhile (> 0)
    & sum

-- | Compute @(n `'choose'`  m) `'mod'` p@ in \(O(\log n)\) time.
chooseModP :: KnownNat p => FactorialModP p -> Integer -> Integer -> Mod p
chooseModP _ n m | n < 0 || m < 0 || m > n = 0
chooseModP fmp n m =
  let p = getP fmp
   in if divideOutFactorialP n p
        == divideOutFactorialP m p + divideOutFactorialP (n - m) p
        then
          factorialNoPModP fmp n
            `quot` (factorialNoPModP fmp m * factorialNoPModP fmp (n - m))
        else 0

-- | A data structure that allows finding @('factorial' n) `'mod'` p@ quickly.
data FactorialModP2 p = FactorialModP2
  { -- | (factorial i) (mod (p^2))
    fmp2Factorial :: Vector (Mod (p ^ 2)),
    -- | (inv 1 + inv 2 + .. + inv i) (mod (p^2))
    fmp2SigmaRecip :: Vector (Mod (p ^ 2)),
    -- | product from j = 0..i-1 of (1 + j*p*sigmaRecip (p1))
    fmp2Magic :: Vector (Mod (p ^ 2))
  }
  deriving (Eq, Show, Ord)

instance HasP (FactorialModP2 p) where
  getP = toInteger . Vector.length . fmp2Factorial

-- | Generate a 'FactorialModP2' in \(O(p)\) time.
mkFactorialModP2 :: forall p. KnownNat p => FactorialModP2 p
mkFactorialModP2 =
  let p :: Int
      p = fromIntegral . toNatural $ (sNat :: SNat p)
      factVec = Vector.fromListN p $ scanl' (*) 1 [1 ..]
      sigmaRecipVec =
        Vector.fromListN p $
          scanl' (+) 0 $
            map (quot 1) [1 ..]
      magicVec =
        Vector.fromListN (p + 1) $
          scanl' (*) 1 $
            flip map [(0 :: Integer) ..] $ \j ->
              1 + fromIntegral j * fromIntegral p * (sigmaRecipVec ! (p - 1))
   in FactorialModP2 factVec sigmaRecipVec magicVec

quotRem2 :: Integer -> Integer -> (Integer, Integer, Integer)
quotRem2 n p = (a, b, c)
  where
    (a, bc) = n `quotRem` square p
    (b, c) = bc `quotRem` p

-- | The product of the numbers less than @n@ relatively prime to @p@, modulo
--   @p^2@.
factorialRelPrimeModP2 ::
  forall p. KnownNat p => FactorialModP2 p -> Integer -> Mod (p ^ 2)
factorialRelPrimeModP2 fmp2 n =
  let p = getP fmp2
      (a, b, c) = quotRem2 n p
      p', b' :: Mod (p ^ 2)
      p' = fromInteger p
      b' = fromInteger b

      fmpFact :: Integer -> Mod (p ^ 2)
      fmpFact i = fmp2Factorial fmp2 ! fromInteger i

      fmpSigmaRecip :: Integer -> Mod (p ^ 2)
      fmpSigmaRecip i = fmp2SigmaRecip fmp2 ! fromInteger i

      fmpMagic :: Integer -> Mod (p ^ 2)
      fmpMagic i = fmp2Magic fmp2 ! fromInteger i

      part1 :: Mod (p ^ 2)
      part1 = fmpFact (p - 1) ^ p * fmpMagic p

      part2 :: Mod (p ^ 2)
      part2 = fmpFact (p - 1) ^ b * fmpMagic b

      part3 :: Mod (p ^ 2)
      part3 = fmpFact c * (1 + b' * p' * fmpSigmaRecip c)
   in part3 * part2 * (part1 ^ a)

-- | The factorial of @n@, without the factors of @p@, modulo @p^2@.
factorialNoPModP2 :: KnownNat p => FactorialModP2 p -> Integer -> Mod (p ^ 2)
factorialNoPModP2 fmp2 n =
  let p = getP fmp2
   in iterate (`quot` p) n
        & takeWhile (> 0)
        & map (factorialRelPrimeModP2 fmp2)
        & product

-- | Compute @(n `'choose'`  m) `'mod'` p@ in \(O(\log p)\) time.
chooseModP2 ::
  KnownNat p => FactorialModP2 p -> Integer -> Integer -> Mod (p ^ 2)
chooseModP2 _ n m | n < 0 || m < 0 || m > n = 0
chooseModP2 fmp2 n m =
  let p = getP fmp2
   in case divideOutFactorialP n p
        - (divideOutFactorialP m p + divideOutFactorialP (n - m) p) of
        0 ->
          factorialNoPModP2 fmp2 n
            `quot` (factorialNoPModP2 fmp2 m * factorialNoPModP2 fmp2 (n - m))
        1 ->
          fromIntegral p
            * ( factorialNoPModP2 fmp2 n
                  `quot` ( factorialNoPModP2 fmp2 m
                             * factorialNoPModP2 fmp2 (n - m)
                         )
              )
        _ -> 0
