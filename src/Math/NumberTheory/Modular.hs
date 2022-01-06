{-|
    Module      : Math.NumberTheory.Modular
    Description : Functions to perform modular arithmetic.
    Copyright   : (c) Preetham Gujjula, 2016 - 2020
    License     : GPL-3
    Maintainer  : pgujjula+hilbert@protonmail.com
    Stability   : experimental

    Functions to perform modular arithmetic.
-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.Modular
    ( egcd
    , chineseRemainder
    , chineseRemainderF
    ) where

import Data.Mod (Mod, invertMod, unMod)
import Data.Type.Natural (SNat, withSNat)

import Control.Arrow                  ((>>>))
import Control.Monad                  (foldM, (>=>))
import Data.Maybe                     (fromJust)
import Data.Ord (comparing)
import Data.Foldable                  (foldl')
import Data.List.Duplicate            (groupBy)

import Math.NumberTheory.Prime.Factor (Factorization, factor)

{-| @egcd m n@ is the extended GCD of @m@ and @n@. It is a tuple @(g, (a, b))@
    such that @gcd m n == g@ and @a*m + b*n == g@.

    >>> egcd 3 5
    (1, (2, -1))
    >>> egcd 10 4
    (2, (1, -2))
    >>> egcd (-4) 7
    (1, (-2, -1))
    >>> egcd 4 0
    (4, (1, 0))
    >>> egcd 0 0
    (0, (0, 0))
-}
egcd :: (Integral a) => a -> a -> (a, (a, a))
egcd m n
  | m < n  = (g', (signa * a', signb * b'))
  | otherwise = (g', (signb * b', signa * a'))
  where (g', (a', b')) = egcdLowLevel (abs lower) (abs upper)
        signa = signum lower
        signb = signum upper
        lower = min m n
        upper = max m n

{- Here, we are guaranteed n > m > 0 -}
egcdLowLevel :: (Integral a) => a -> a -> (a, (a, a))
egcdLowLevel m n = egcdRecursive m n (1, 0) (0, 1)

egcdRecursive :: (Integral a) => a -> a -> (a, a) -> (a, a) -> (a, (a, a))
egcdRecursive m n m_coeffs n_coeffs
    | m == 0 = (n, n_coeffs)
    | otherwise = egcdRecursive r m (n0 - q*m0, n1 - q*m1) m_coeffs
  where
    (q, r) = quotRem n m
    (m0, m1) = m_coeffs
    (n0, n1) = n_coeffs

{-| Solve a system of modular congruences. The input list is a list of pairs
    @(ai, mi)@, representing a system

    \[\forall i \quad x \equiv a_{i} \mod m_{i} \]

    The result @(x, m)@ is the solution @x@ and its modulus @m@, if a
    solution exists.

    >>> EXAMPLE HERE
-}
chineseRemainder :: Integral a => [(a, a)] -> Maybe (a, a)
chineseRemainder = traverse (\(x, m) -> (x,) <$> factor (abs m))
               >=> chineseRemainderF

{-| Like 'chineseRemainder', but takes in the factorizations of the moduli to
    speed up computation.

    >>> EXAMPLE HERE
-}
chineseRemainderF :: Integral a => [(a, Factorization a)] -> Maybe (a, a)
chineseRemainderF = breakUp >>> massage >>> fmap (uncurry solveAllPrime)
                >>> sequence >>> fmap solveAll

breakUp :: [(a, Factorization a)] -> [(a, (a, Int))]
breakUp = concatMap (\(x, fact) -> fmap (x,) fact)

massage :: Ord b => [(a, (b, c))] -> [(b, [(a, c)])]
massage = fmap (\(a, (b, c)) -> (b, (a, c)))
      >>> groupBy (comparing fst)
      >>> fmap (\xs -> (fst (head xs), fmap snd xs))

solveAll :: Integral a => [(a, a)] -> (a, a)
solveAll = foldl' solve (0, 1)

solve :: Integral a => (a, a) -> (a, a) -> (a, a)
solve (a, m) (b, n) = (x, s)
  where
    s = m*n
    x = ((a * (fromJust $ modInv n m)) * n
       + (b * (fromJust $ modInv m n)) * m)
       `rem` s

modInv :: Integral a => a -> a -> Maybe a
modInv a m = withSNat (fromIntegral m) $ \(_ :: SNat n) ->
  fmap (fromIntegral . unMod) (invertMod (fromIntegral a :: Mod n))

-- Given a prime p and a list of (xi, ki) representing a system of congruences
--    x == xi (mod p^ki)
-- solve for x, k such that x (mod p^k) is a solution to the system.
solveAllPrime :: Integral a => a -> [(a, Int)] -> Maybe (a, a)
solveAllPrime p = fmap (\(x, k) -> (x, p^k))
                . foldM (solvePrime p) (0, 0)

solvePrime :: Integral a => a -> (a, Int) -> (a, Int) -> Maybe (a, Int)
solvePrime p (x1, k1) (x2, k2)
    | k1 > k2 = solvePrime p (x2, k2) (x1, k1)
    | (x2 - x1) `mod` (p^k1) /= 0 = Nothing
    | otherwise = Just (x2, k2)
