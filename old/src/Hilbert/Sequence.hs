{-|
    Module      : Hilbert.List
    Description : Interesting sequences.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Interesting sequences
-}

module Hilbert.Sequence
  ( fibonacci
  ) where

{-|
    @fibonacci@ is a list of the fibonacci numbers, starting with 0 and 1.

    >>> take 8 fibonacci
    [0, 1, 1, 2, 3, 5, 8, 13]
-}
fibonacci :: (Integral a) => [a]
fibonacci = fibsFrom 0 1
  where fibsFrom a b = a:(fibsFrom b $! (a + b))
