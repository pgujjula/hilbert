{-|
    Module      : Hilbert.Prime.Factor.Type
    Description : Prime factorization type
    Copyright   : (c) Preetham Gujjula, 2019
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Prime factorization type.
-}

module Hilbert.Prime.Factor.Type
  ( Factorization
  ) where

type Factorization a = [(a, Int)]
