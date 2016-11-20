{-|
    Module      : Hilbert.Prime.List.Erastosthenes
    Description : List of prime numbers.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Generate lists of prime numbers. This module uses mathematics described in
    
        * Neill, Melissa O. "The Genuine Sieve of Eratosthenes." Journal of
          Functional Programming 19.1 (2009): 95-106. Cambridge University
          Press. Web.
-}

module Hilbert.Prime.List.Erastosthenes
  ( primes
  , primesTo
  , primesTo'
  ) where

import Hilbert.Prime.List.Erastosthenes.Lazy (primes, primesTo)
import Hilbert.Prime.List.Erastosthenes.Strict (primesTo')
