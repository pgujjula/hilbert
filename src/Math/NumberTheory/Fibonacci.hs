{-| Module      : Math.NumberTheory.ContinuedFraction
    Description : Fibonacci and Lucas numbers, and their generalization.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Fibonacci and Lucas numbers, and their generalization.
-}

module Math.NumberTheory.Fibonacci
    ( -- * Fibonacci numbers
      fibonaccis
    , fibonaccisMod
    , fibonacciN
    , fibonacciModN

    -- * Lucas numbers
    , lucasNums
    , lucasNumsMod
    , lucasNumN
    , lucasNumModN

    -- * Generalized Lucas sequences
    -- | See <https://en.wikipedia.org/wiki/Lucas_sequence Wikipedia> for a
    --   definition of /U/ and /V/.
    , lucasSeq
    , lucasSeqMod
    , lucasSeqN
    , lucasSeqModN
    ) where

{-| The Fibonacci numbers.

    >>> take 10 fibonaccis
    [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
-}
fibonaccis :: (Integral a) => [a]
fibonaccis = fst $ lucasSeq 1 (-1)

{-| The Fibonacci numbers, modulo some base. Calls 'error' when the base is 0.

    >>> take 10 (fibonaccisMod 10)
    [0, 1, 1, 2, 3, 5, 8, 3, 1, 4]
-}
fibonaccisMod :: (Integral a) => a -> [a]
fibonaccisMod m = fst $ lucasSeqMod m 1 (-1)

{-| The @n@th Fibonacci number. Calls 'error' when @n@ is less than 0.

    >>> map fibonacciN [0..9]
    [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
-}
fibonacciN :: (Integral a, Integral b) => b -> a
fibonacciN = fst . lucasSeqN 1 (-1)

{-| @fibonacciModN m n@ is the @n@th Fibonacci number, modulo @m@. Calls 'error'
    when @m@ is zero or @n@ is less than 0.

    >>> map (fibonacciModN 10) [0..9]
    [0, 1, 1, 2, 3, 5, 8, 3, 1, 4]
-}
fibonacciModN :: (Integral a, Integral b) => a -> b -> a
fibonacciModN m = fst . lucasSeqModN m 1 (-1)

{-| The Lucas numbers.

    >>> take 10 lucasNums
    [2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
-}
lucasNums :: (Integral a) => [a]
lucasNums = snd $ lucasSeq 1 (-1)

{-| The Lucas numbers, modulo some base. Calls 'error' when the base is 0.

    >>> take 10 (lucasNumsMod 10)
    [2, 1, 3, 4, 7, 1, 8, 9, 7, 6]
-}
lucasNumsMod :: (Integral a) => a -> [a]
lucasNumsMod m = snd $ lucasSeqMod m 1 (-1)

{-| The @n@th Lucas number. Calls 'error' when @n@ is less than 0.

    >>> map lucasN [0..9]
    [2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
-}
lucasNumN :: (Integral a, Integral b) => b -> a
lucasNumN = snd . lucasSeqN 1 (-1)

{-| @lucasModN m n@ is the @n@th Lucas number, modulo @m@. Calls 'error'
    when @m@ is zero or @n@ is less than 0.

    >>> map (lucasNumModN 10) [0..9]
    [2, 1, 3, 4, 7, 1, 8, 9, 7, 6]
-}
lucasNumModN :: (Integral a, Integral b) => a -> b -> a
lucasNumModN m = snd . lucasSeqModN m 1 (-1)

go :: (Integral a) => (a -> a -> a) -> a -> a -> [a]
go f a b = a : seq c (go f b c)
  where
    c = f a b

{-| The /U/ and /V/ sequences associated with seeds /p/ and /q/.
    
    >>> let (p, q) = (1, -1)
    >>> let (u, v) = lucasSeq p q
    >>> take 10 u
    [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
    >>> take 10 v
    [2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
-}
lucasSeq :: (Integral a) => a -> a -> ([a], [a])
lucasSeq p q = (go f 0 1, go f 2 p)
  where
    f x y = -q*x + p*y

{-| The /U/ and /V/ sequences associated with seeds /p/ and /q/, modulo a base.

    >>> let modulus = 10
    >>> let (p, q) = (1, -1)
    >>> let (u, v) = lucasSeqMod modulus p q
    >>> take 10 u
    [0, 1, 1, 2, 3, 5, 8, 3, 1, 4]
    >>> take 10 v
    [2, 1, 3, 4, 7, 1, 8, 9, 7, 6]
-}
lucasSeqMod :: (Integral a) => a -> a -> a -> ([a], [a])
lucasSeqMod m p q = (go f u0 u1, go f v0 v1)
  where
    f x y = (q'*x + p'*y) `rem` m'
    m' = abs m
    p' = p `mod` m'
    q' = (-q) `mod` m'

    u0 = 0
    u1 = 1 `rem` m'

    v0 = 2 `rem` m'
    v1 = p'

-- Represent the nth and (n + 1)th Lucas U- and V- numbers
data Group a = Group !a !a !a !a
  deriving (Eq, Show, Ord)

step :: (Integral a) => (a -> a) -> a -> a -> Group a -> Group a
step reduce p q (Group un un1 vn vn1) = Group un1 un2 vn1 vn2
  where
    un2 = reduce $ (-q)*un + p*un1
    vn2 = reduce $ (-q)*vn + p*vn1

double :: Integral a => (a -> a) -> (a -> b -> a) -> a -> a -> b -> Group a -> Group a
double reduce pow p q n (Group un un1 vn vn1) = Group u2n u2n1 v2n v2n1
  where
    u2n  = reduce $ un * vn
    u2n1 = reduce $ un1*vn - qToN
    v2n  = reduce $ vn*vn - 2 * qToN
    v2n1 = reduce $ vn1*vn - p * qToN
    qToN = pow q n

{-| The @n@th terms of the /U/ and /V/ sequences associated with seeds /p/ and
    /q/.
    
    >>> let (p, q) = (1, -1)
    >>> let n = 9
    >>> let (u, v) = lucasSeqN p q n
    >>> u
    34
    >>> v
    76
-}
lucasSeqN :: (Integral a, Integral b) => a -> a -> b -> (a, a)
lucasSeqN p q = (\(Group u _ v _) -> (u, v)) . mkGroup
  where
    mkGroup n
        | n == 0    = Group 0 1 2 p
        | odd n     = step id p q (mkGroup (n - 1))
        | otherwise = double id (^) p q halfN (mkGroup halfN)
      where
        halfN = n `div` 2

{-| The @n@th terms of the /U/ and /V/ sequences associated with seeds /p/ and
    /q/, modulo some base
    
    >>> let modulus = 10
    >>> let (p, q) = (1, -1)
    >>> let n = 9
    >>> let (u, v) = lucasSeqN modulus p q n
    >>> u
    4
    >>> v
    6
-}
lucasSeqModN :: (Integral a, Integral b) => a -> a -> a -> b -> (a, a)
lucasSeqModN m p q = (\(Group u _ v _) -> (u, v)) . mkGroup
  where
    mkGroup n
        | n == 0    = Group u0 u1 v0 v1
        | odd n     = step reduce p' q' (mkGroup (n - 1))
        | otherwise = double reduce (^) p' q' halfN (mkGroup halfN)
      where
        halfN = n `div` 2

    m' = abs m
    reduce = (`mod` m')
    p' = reduce p
    q' = reduce q

    u0 = 0
    u1 = reduce 1
    v0 = reduce 2
    v1 = p'
