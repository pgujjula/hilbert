module Hilbert.Legendre where

import Hilbert.Modular (modPow)

legendre a p = if r == (p - 1)
               then -1
               else r
               where r = modPow a ((p - 1) `div` 2) p

jacobi 1 n = 1
jacobi n 1 = 1
jacobi 0 n = 0
jacobi 2 n = (-1)^(((n^2 - 1) `div` 8) `rem` 2)
jacobi a n | a < 0 = ((-1)^(((n - 1) `div` 2))) * (jacobi (-a) n)
jacobi a n | a > n = jacobi (a `rem` n) n
jacobi a n | even a = (jacobi 2 n) * (jacobi (a `div` 2) n)
jacobi a n | gcd a n > 1 = 0
jacobi a n = (jacobi n a)*((-1)^((a - 1)*(n - 1) `div` 4))
