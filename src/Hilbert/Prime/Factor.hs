{-|
    Module      : Hilbert.Prime.Factor
    Description : Prime factorization
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Prime factorization.
-}
{-# LANGUAGE FlexibleContexts #-}
module Hilbert.Prime.Factor
  ( factor
  , factorUpTo
  , factorUpTo'
  , factorizations
  ) where

import Hilbert.Prime.Factor.Single
import Hilbert.Prime.Factor.List.Lazy
import Hilbert.Prime.Factor.List.Strict
