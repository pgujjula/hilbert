{-|
    Module      : Hilbert.Sequence
    Description : Sequences of numbers.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Sequences of numbers. The section on Lucas sequences uses mathematics in
    
        * Riesel, Hans. /Prime Numbers and Computer Methods for Factorization/.
          2nd ed., Springer Science+Business Media, 1994, pp. 107-109.
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Hilbert.Sequence 
    ( fibonacci
    , fibonacciAt
    , fibonacciAtMod

    , lucas
    , lucasAt
    , lucasAtMod

    , lucasU
    , lucasUAt
    , lucasUAtMod

    , lucasV
    , lucasVAt
    , lucasVAtMod
    ) where

import Hilbert.Modular (modPow)

{-|
    List of the Fibonacci numbers. Equivalent to @'lucasU' 1 (-1)@.

    >>> take 10 fibonacci
    [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
-}
fibonacci :: (Integral a) => [a]
fibonacci = lucasU 1 (-1)

{-|
    @'fibonacciAt' n@ is the @n@th Fibonacci number. Equivalent to
    @'lucasUAt' 1 (-1) n@.

    __Preconditions:__ @n@ is nonnegative.

    >>> fibonacciAt 6
    8
-}
fibonacciAt :: (Integral a) => a -> a
fibonacciAt = lucasUAt 1 (-1)

{-|
    @'fibonacciAtMod' n m@ is the @n@th Fibonacci number modulo m. Equivalent
    to @'lucasUAtMod' 1 (-1) n m@. 

    __Preconditions:__
    
        * @n@ is nonnegative.
        * @m@ is positive.

    Since @fibonacciAt 9 == 34@,

    >>> fibonacciAtMod 9 10
    4
-}
fibonacciAtMod :: (Integral a) => a -> a -> a
fibonacciAtMod = lucasUAtMod 1 (-1)

{-|
    List of the Lucas numbers. Equivalent to @'lucasV' 1 (-1)@.

    >>> take 10 lucas
    [2, 1, 3, 4, 7, 11, 18, 29, 47, 76]
-}
lucas :: (Integral a) => [a]
lucas = lucasV 1 (-1)

{-|
    @'lucasAt' n@ is the @n@th Lucas number. Equivalent to
    @'lucasVAt' 1 (-1) n@.

    __Preconditions:__ @n@ is nonnegative.

    >>> lucasAt 6
    18
-}
lucasAt :: (Integral a) => a -> a
lucasAt = lucasVAt 1 (-1)

{-|
    @'lucasAtMod' n m@ is the @n@th Fibonacci number modulo m. Equivalent
    to @'lucasVAtMod' 1 (-1) n m@. 

    __Preconditions:__
    
        * @n@ is nonnegative.
        * @m@ is positive.

    Since @lucasAtMod 9 == 76@,

    >>> lucasAtMod 9 10
    6
-}
lucasAtMod :: (Integral a) => a -> a -> a
lucasAtMod = lucasVAtMod 1 (-1)

{-|
    @lucasU p q@ is the lucas U sequence with initial conditions p and
    q.

    __Preconditions:__ None.

    >>> take 5 $ lucasU 1 2
    [0, 1, 1, -1, -3]
-}
lucasU :: (Integral a) => a -> a -> [a]
lucasU p q = sequence
    where sequence = [0, 1]
                ++ (zipWith (+) (map (p*)    (drop 1 sequence))
                                (map ((-q)*)  sequence))

{-|
    @lucasUAt p q n@ is equivalent to, but faster than,
    @(lucasU p q) !! n@.

    __Preconditions:__ @n@ is nonnegative.

    >>> lucasUAt 1 2 4
    -3
-}
lucasUAt :: forall a. (Integral a) => a -> a -> a -> a
lucasUAt p q n = (\(x, _, _, _) -> x) (four p q n)

{-|
    @lucasUAtMod p q n m@ is equivalent to, but (much) faster than,
    @mod (lucasUAt p q n) m@. Useful for implementing the Lucas
    probable prime test.

    __Preconditions:__

        * @n@ is nonnegative.
        * @m@ is positive.
    
    Since @lucasUAt 4 1 5 == 153@,

    >>> lucasUAtMod 4 1 5 10
    3
-}
lucasUAtMod :: (Integral a) => a -> a -> a -> a -> a
lucasUAtMod p q n m = fromIntegral $ lucasUAtMod_integer
                        (toInteger p) (toInteger q)
                        (toInteger n) (toInteger m)

lucasUAtMod_integer :: Integer -> Integer -> Integer -> Integer -> Integer
lucasUAtMod_integer p q n m = (\(x, _, _, _) -> x) (fourMod p q n m)

{-|
    @lucasV p q@ is the lucas V sequence with initial conditions p and
    q.

    __Preconditions:__ None.

    >>> take 5 $ lucasV 1 2
    [2, 1, -3, -5, -11]
-}
lucasV :: (Integral a) => a -> a -> [a]
lucasV p q = sequence
    where sequence = [2, p]
                ++ (zipWith (+) (map (p*)    (drop 1 sequence))
                                (map ((-q)*)  sequence))

{-|
    @lucasVAt p q n@ is equivalent to, but faster than,
    @(lucasV p q) !! n@.

    __Preconditions:__ @n@ is nonnegative.

    >>> lucasVAt 1 2 4
    -11
-}
lucasVAt :: (Integral a) => a -> a -> a -> a
lucasVAt p q n = (\(_, _, x, _) -> x) (four p q n)

{-|
    @lucasVAtMod p q n m@ is equivalent to, but (much) faster than,
    @mod (lucasVAt p q n) m@. Useful for implementing the Lucas
    probable prime test.

    __Preconditions:__

        * @n@ is nonnegative.
        * @m@ is positive.
    
    Since @lucasVAt 4 1 5 == 194@,

    >>> lucasVAtMod 4 1 5 10
    4
-}
lucasVAtMod :: (Integral a) => a -> a -> a -> a -> a
lucasVAtMod p q n m = fromIntegral $ lucasVAtMod_integer
                        (toInteger p) (toInteger q)
                        (toInteger n) (toInteger m)

lucasVAtMod_integer :: Integer -> Integer -> Integer -> Integer -> Integer
lucasVAtMod_integer p q n m = (\(_, _, x, _) -> x) (fourMod p q n m)

{-
   four p q n computes lucasUAt p q n, lucasUAt p q (n + 1), lucasVAt p q n,
   and lucaseVAt p q (n + 1).
-}
four :: (Integral a) => a -> a -> a -> (a, a, a, a)
four p q 0    = (0, 1, 2, p)
four p q n
    | even n = (u2k, u2k1, v2k, v2k1)
    | otherwise = (u2k1, u2k2, v2k1, v2k2)
    -- uk = u_{k}, u2k1 = u_{2k + 1}, etc.
    where (uk, uk1, vk, vk1) = four p q k
          u2k  = uk * vk
          u2k1 = uk1 * vk - qk
          u2k2 = uk1*vk1
          v2k  = vk*vk - 2*qk
          v2k1 = vk1 * vk - p * qk
          v2k2 = vk1*vk1 - 2*(qk * q)
          qk = q^k
          k = n `div` 2

{-
   fourMod p q n m is equivalent to (fourMod p q n) `mod` m, but much faster.
-}
fourMod :: (Integral a) => a -> a -> a -> a -> (a, a, a, a)
fourMod p q n m
    | n == 0 = (0, reduce 1, reduce 2, reduce p)
    | even n = (u2k, u2k1, v2k, v2k1)
    | otherwise = (u2k1, u2k2, v2k1, v2k2)
    -- uk = u_{k}, u2k1 = u_{2k + 1}, etc.
    where (uk, uk1, vk, vk1) = fourMod p q k m
          u2k  = reduce $ uk * vk
          u2k1 = reduce $ uk1 * vk - qk
          u2k2 = reduce $ uk1*vk1
          v2k  = reduce $ vk*vk - 2*qk
          v2k1 = reduce $ vk1 * vk - p * qk
          v2k2 = reduce $ vk1*vk1 - 2*(qk * q)
          qk = modPow q k m
          k = n `div` 2
          reduce x = x `mod` m
