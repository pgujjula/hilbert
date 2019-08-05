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
  , Factorization
  ) where

import qualified Hilbert.Prime.Check.TrialDivision
    as Check.TrialDivision (isPrime)
import qualified Hilbert.Prime.Check.Probable.MillerRabin
    as Check.Probable.MillerRabin (isProbablePrime)
import qualified Hilbert.Prime.List.Erastosthenes.Lazy
    as List.Erastosthenes.Lazy (primes, primesTo)
import qualified Hilbert.Prime.List.Erastosthenes.Strict
    as List.Erastosthenes.Strict (primesTo')
import qualified Hilbert.Prime.Factor.TrialDivision
    as Factor.TrialDivision (factor)
import qualified Hilbert.Prime.Factor.List.Lazy
    as Factor.List.Lazy (factorTo, factorToInf)
import qualified Hilbert.Prime.Factor.List.Strict
    as Factor.List.Strict (factorTo')
import Hilbert.Prime.Factor.Type (Factorization)

{-| 
    Determine whether a number is prime. Currently uses
    trial division: 'Check.TrialDivision.isPrime'. 

    __Preconditions:__ None.
-}
isPrime :: (Integral a) => a -> Bool
isPrime = Check.TrialDivision.isPrime

{-|
    Determine whether a number is a prime using a probabalistic test. The
    answer might occasionally be incorrect. Currently uses the Miller-Rabin
    test: 'Check.Probable.MillerRabin.isProbablePrime'.

    __Preconditions:__ None
    -}
isProbablePrime = Check.Probable.MillerRabin.isProbablePrime

{-|
    A lazily generated infinite list of primes. Currently uses the Sieve of
    Erastosthenes: 'List.Erastosthenes.Lazy.primes'.
-}
primes          = List.Erastosthenes.Lazy.primes

{-|
    Generate a list of primes up to the given input, computed lazily. Currently
    uses the Sieve of Erastosthenes: 'List.Erastosthenes.Lazy.primesTo'.

    __Precondition:__ @n@ is nonnegative.
-}
primesTo        = List.Erastosthenes.Lazy.primesTo

{-|
    Generate a list of primes up to the given input, computed strictly.
    Currently uses the Sieve of Erastosthenes:
    'List.Erastosthenes.Strict.primesTo''.

    __Precondition:__ @n@ is nonnegative.
-}
primesTo'       = List.Erastosthenes.Strict.primesTo'

{-|
    Factor an integer. Currently uses trial division: 'Factor.TrialDivision.factor'.

    __Precondition:__ Input is nonzero.

-}
factor          = Factor.TrialDivision.factor

{-|
    Factor all the integers up to the input, lazily: 'Factor.List.Lazy.factorTo'.

    __Precondition:__ Input is nonnegative.
-}
factorTo        = Factor.List.Lazy.factorTo

{-|
    Factor all the integers up to the input, strictly: 'Factor.List.Strict.factorTo''.

    __Precondition:__ Input is nonnegative.
-}
factorTo'       = Factor.List.Strict.factorTo'

{-|
    A lazily generated list of the factorizations of all positive integers:
    'Factor.List.Lazy.factorToInf'.
-}
factorToInf     = Factor.List.Lazy.factorToInf
