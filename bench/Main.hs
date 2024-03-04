-- |
-- Copyright   : 2022 Preetham Gujjula
-- License     : BSD-3-Clause
-- Maintainer  : libraries@mail.preetham.io
-- Stability   : experimental
module Main (main) where

import Data.Function                  ((&))
import Math.NumberTheory.Divisor      (mobiusesFrom)
import Math.NumberTheory.Prime.Factor (factorizationsFrom)
import Test.Tasty.Bench

factorizationsFromBenchmark :: Benchmark
factorizationsFromBenchmark = bgroup "factorizationsFrom" (fmap mkBench [1..7])
  where
    mkBench :: Int -> Benchmark
    mkBench expo =
      bench
        ("take (10^" ++ show expo ++ ") (factorizationsFrom 1)")
        (nf collapseFactorizations (10^expo))

    collapseFactorizations :: Int -> Int
    collapseFactorizations n =
      -- need this trick to ensure GHC recomputes this function every time
      factorizationsFrom ((n `quot` maxBound)+1)
      & take n
      & map (sum . map (uncurry (+)))
      & sum

mobiusesFromBenchmark :: Benchmark
mobiusesFromBenchmark = bgroup "mobiusesFrom" (map mkBench [1..8])
  where
    mkBench :: Int -> Benchmark
    mkBench expo =
      bench
        ("take (10^" ++ show expo ++ ") (mobiusesFrom 1)")
        (nf collapseMobiuses (10^expo))

    collapseMobiuses :: Int -> Int
    collapseMobiuses n =
      mobiusesFrom ((n `quot` maxBound)+1)
      & take n
      & sum

main :: IO ()
main = defaultMain
  [ factorizationsFromBenchmark
  , mobiusesFromBenchmark
  ]
