{-|
    Module      : Hilbert.Prime.Factor.List
    Description : Factor the positive integers lazily.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Factor lists of the positive integers.
-}

module Hilbert.Prime.Factor.List
  ( factorTo
  , factorTo'
  , factorToInf
  ) where

import Hilbert.Prime.Factor.List.Lazy (factorTo, factorToInf)
import Hilbert.Prime.Factor.List.Strict (factorTo')
