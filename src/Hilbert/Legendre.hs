{-|
    Module      : Hilbert.Legendre
    Description : Legendre and Jacobi symbols.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Compute Legendre and Jacobi symbols.
-}
module Hilbert.Legendre
  ( legendre
  , jacobi
  ) where

import Hilbert.Modular (modPow)

{-|
    @legendre a p@ computes the Legendre symbol (a/p).
    Undefined output if @p@ is not prime

    >>> legendre 10 7
    -1
    >>> legendre 7 7
    0
    >>> legendre 16 11
    1
-}
legendre :: (Integral a) => a -> a -> a
legendre a p = if r == (p - 1)
               then -1
               else r
               where r = modPow a ((p - 1) `div` 2) p

{-|
    @jacobi a n@ computes the Jacobi symbol (a/n).
    Undefined output if @n@ is not a positive odd number

    >>> jacobi 0 3
    0
    >>> jacobi 8 1
    1
    >>> jacobi 7 15
    -1
    >>> jacobi (-3) 35
    -1
-}
jacobi :: (Integral a) => a -> a -> a
jacobi 1 n = 1
jacobi n 1 = 1
jacobi 0 n = 0
jacobi 2 n = (-1)^(((n^2 - 1) `div` 8) `rem` 2)
jacobi a n | a < 0 = ((-1)^(((n - 1) `div` 2))) * (jacobi (-a) n)
jacobi a n | a > n = jacobi (a `rem` n) n
jacobi a n | even a = (jacobi 2 n) * (jacobi (a `div` 2) n)
jacobi a n | gcd a n > 1 = 0
jacobi a n = (jacobi n a)*((-1)^((a - 1)*(n - 1) `div` 4))
