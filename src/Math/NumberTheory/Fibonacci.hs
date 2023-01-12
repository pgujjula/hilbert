-- | Module      : Math.NumberTheory.Fibonacci
--   Description : Fibonacci and Lucas numbers, and their generalization.
--   Copyright   : (c) Preetham Gujjula, 2020
--   License     : BSD-3-Clause
--   Maintainer  : preetham.gujjula@gmail.com
--   Stability   : experimental
--
--   Fibonacci and Lucas numbers, and their generalization.
module Math.NumberTheory.Fibonacci
  ( -- * Fibonacci numbers
    fibonaccis,
    fibonacciN,

    -- * Lucas numbers
    lucasNums,
    lucasNumN,

    -- * Generalized Lucas sequences

    -- | See <https://en.wikipedia.org/wiki/Lucas_sequence Wikipedia> for a
    --   definition of /U/ and /V/.
    lucasSeq,
    lucasSeqN,
  )
where

-- | The Fibonacci numbers.
--
--   >>> take 10 fibonaccis
--   [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
fibonaccis :: (Num a) => [a]
fibonaccis = fst $ lucasSeq 1 (-1)

-- | The @n@th Fibonacci number. Calls 'error' when @n@ is less than 0.
--
--   >>> map fibonacciN [0..9]
--   [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
fibonacciN :: (Num a, Integral b) => b -> a
fibonacciN = fst . lucasSeqN 1 (-1)

-- | The Lucas numbers.
--
--   >>> take 10 lucasNums
--   [2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
lucasNums :: (Num a) => [a]
lucasNums = snd $ lucasSeq 1 (-1)

-- | The @n@th Lucas number. Calls 'error' when @n@ is less than 0.
--
--   >>> map lucasN [0..9]
--   [2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
lucasNumN :: (Num a, Integral b) => b -> a
lucasNumN = snd . lucasSeqN 1 (-1)

go :: (Num a) => (a -> a -> a) -> a -> a -> [a]
go f a b = a : seq c (go f b c)
  where
    c = f a b

-- | The /U/ and /V/ sequences associated with seeds /p/ and /q/.
--
--   >>> let (p, q) = (1, -1)
--   >>> let (u, v) = lucasSeq p q
--   >>> take 10 u
--   [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
--   >>> take 10 v
--   [2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
lucasSeq :: (Num a) => a -> a -> ([a], [a])
lucasSeq p q = (go f 0 1, go f 2 p)
  where
    f x y = -q * x + p * y

-- Represent the nth and (n + 1)th Lucas U- and V- numbers
data Group a = Group !a !a !a !a
  deriving (Eq, Show, Ord)

step :: (Num a) => a -> a -> Group a -> Group a
step p q (Group un un1 vn vn1) = Group un1 un2 vn1 vn2
  where
    un2 = (-q) * un + p * un1
    vn2 = (-q) * vn + p * vn1

double :: Num a => (a -> b -> a) -> a -> a -> b -> Group a -> Group a
double pow p q n (Group un un1 vn vn1) = Group u2n u2n1 v2n v2n1
  where
    u2n = un * vn
    u2n1 = un1 * vn - qToN
    v2n = vn * vn - 2 * qToN
    v2n1 = vn1 * vn - p * qToN
    qToN = pow q n

-- | The @n@th terms of the /U/ and /V/ sequences associated with seeds /p/ and
--   /q/.
--
--   >>> let (p, q) = (1, -1)
--   >>> let n = 9
--   >>> let (u, v) = lucasSeqN p q n
--   >>> u
--   34
--   >>> v
--   76
lucasSeqN :: (Num a, Integral b) => a -> a -> b -> (a, a)
lucasSeqN p q = (\(Group u _ v _) -> (u, v)) . mkGroup
  where
    mkGroup n
      | n == 0 = Group 0 1 2 p
      | odd n = step p q (mkGroup (n - 1))
      | otherwise = double (^) p q halfN (mkGroup halfN)
      where
        halfN = n `div` 2
