{-| Module      : Math.NumberTheory.Divisor
    Description : Divisors of integers.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Divisors of integers.
-}

module Math.NumberTheory.Divisor
    ( divisors
    , divisorsF
    , divisorPairs
    , divisorPairsF
    ) where

import Data.List (foldl')
import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)

import Math.NumberTheory.Prime.Factor (Factorization, factor)

divisors :: (Integral a) => a -> [a]
divisors = fromMaybe [] . fmap divisorsF . factor . abs

divisorsF :: (Integral a) => Factorization a -> [a]
divisorsF = foldl' (liftA2 (*)) [1] . map pows
  where
    pows (p, e) = take (e + 1) $ iterate (*p) 1

divisorPairs :: (Integral a) => a -> [(a, a)]
divisorPairs = mkPairs . divisors

divisorPairsF :: (Integral a) => Factorization a -> [(a, a)]
divisorPairsF = mkPairs . divisorsF

mkPairs :: [a] -> [(a, a)]
mkPairs xs = take ((length xs + 1) `div` 2) $ zip xs (reverse xs)
