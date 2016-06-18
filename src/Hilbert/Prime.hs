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
  ( trialDivision
  , millerRabin
  ) where
import Hilbert.Prime.MillerRabin   (millerRabin)
import Hilbert.Prime.TrialDivision (trialDivision)
