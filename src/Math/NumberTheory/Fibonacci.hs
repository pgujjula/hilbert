{-| Module      : Math.NumberTheory.ContinuedFraction
    Description : Fibonacci and Lucas numbers, and their generalization.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Fibonacci and Lucas numbers, and their generalization.
-}

module Math.NumberTheory.Fibonacci
    ( fibonacci
    , fibonacciMod
    , fibonacciN
    , fibonacciModN

    , lucasNum
    , lucasNumMod
    , lucasNumN
    , lucasNumModN

    , lucasSeq
    , lucasSeqMod
    , lucasSeqN
    , lucasSeqModN
    ) where

fibonacci :: (Integral a) => [a]
fibonacci = fst $ lucasSeq 1 (-1)

fibonacciMod :: (Integral a) => a -> [a]
fibonacciMod m = fst $ lucasSeqMod m 1 (-1)

fibonacciN :: (Integral a) => a -> a
fibonacciN = fst . lucasSeqN 1 (-1)

fibonacciModN :: (Integral a) => a -> a -> a
fibonacciModN m = fst . lucasSeqModN m 1 (-1)


lucasNum :: (Integral a) => [a]
lucasNum = snd $ lucasSeq 1 (-1)

lucasNumMod :: (Integral a) => a -> [a]
lucasNumMod m = snd $ lucasSeqMod m 1 (-1)

lucasNumN :: (Integral a) => a -> a
lucasNumN = snd . lucasSeqN 1 (-1)

lucasNumModN :: (Integral a) => a -> a -> a
lucasNumModN m = snd . lucasSeqModN m 1 (-1)


go :: (Integral a) => (a -> a -> a) -> a -> a -> [a]
go f a b = a : seq c (go f b c)
  where
    c = f a b

lucasSeq :: (Integral a) => a -> a -> ([a], [a])
lucasSeq p q = (go f 0 1, go f 2 p)
  where
    f x y = -q*x + p*y

lucasSeqMod :: (Integral a) => a -> a -> a -> ([a], [a])
lucasSeqMod m p q = (go f 0 1, go f 2 p)
  where
    f x y = (-q*x + p*y) `rem` m

-- Represent the nth and (n + 1)th Lucas U- and V- numbers
data Group a = Group !a !a !a !a
  deriving (Eq, Show, Ord)

step :: (Integral a) => (a -> a) -> a -> a -> Group a -> Group a
step reduce p q (Group un un1 vn vn1) = Group un1 un2 vn1 vn2
  where
    un2 = reduce $ (-q)*un + p*un1
    vn2 = reduce $ (-q)*vn + p*vn1

double :: (Integral a) => (a -> a) -> (a -> a -> a) -> a -> a -> a -> Group a -> Group a
double reduce pow p q n (Group un un1 vn vn1) = Group u2n u2n1 v2n v2n1
  where
    u2n  = reduce $ un * vn
    u2n1 = reduce $ un1*vn - qToN
    v2n  = reduce $ vn*vn - 2 * qToN
    v2n1 = reduce $ vn1*vn - p * qToN
    qToN = pow q n

lucasSeqN :: (Integral a) => a -> a -> a -> (a, a)
lucasSeqN p q = (\(Group u _ v _) -> (u, v)) . mkGroup
  where
    mkGroup n
        | n == 0    = Group 0 1 2 p
        | odd n     = step id p q (mkGroup (n - 1))
        | otherwise = double id (^) p q halfN (mkGroup halfN)
      where
        halfN = n `div` 2

lucasSeqModN :: (Integral a) => a -> a -> a -> a -> (a, a)
lucasSeqModN m p q = (\(Group u _ v _) -> (u, v)) . mkGroup
  where
    mkGroup n
        | n == 0    = Group 0 1 2 p'
        | odd n     = step reduce p' q' (mkGroup (n - 1))
        | otherwise = double reduce (^) p' q' halfN (mkGroup halfN)
      where
        halfN = n `div` 2

    reduce = (`rem` m)
    p' = reduce p
    q' = reduce q
