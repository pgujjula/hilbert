{-|
    Module      : Hilbert.Prime
    Description : Functions related to primes.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Functions related to primes.
-}

module Hilbert.Prime
  ( isPrime
  , isProbablePrime
  , primes
  , primesTo
  , primesTo'
  , factor
  , factorTo
  , factorTo'
  , factorToInf
  ) where

import qualified Hilbert.Prime.Check.TrialDivision
    as TrialDivision (isPrime)
import qualified Hilbert.Prime.Check.Probable.MillerRabin
    as MillerRabin (isProbablePrime)
import qualified Hilbert.Prime.List.Erastosthenes.Lazy
    as Erastosthenes (primes, primesTo)
import qualified Hilbert.Prime.List.Erastosthenes.Strict
    as Erastosthenes (primesTo')

isPrime         = TrialDivision.isPrime
isProbablePrime = MillerRabin.isProbablePrime
primes          = Erastosthenes.primes
primesTo        = Erastosthenes.primesTo
primesTo'       = Erastosthenes.primesTo'
factor          = undefined
factorTo        = undefined
factorTo'       = undefined
factorToInf     = undefined
