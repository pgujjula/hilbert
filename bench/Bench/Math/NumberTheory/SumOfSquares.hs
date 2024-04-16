module Bench.Math.NumberTheory.SumOfSquares (sumOfSquaresBenchmarks) where

import Test.Tasty.Bench
import Data.Function ((&))
import Math.NumberTheory.SumOfSquares (sumOfSquaresF)
import Math.NumberTheory.Prime.Factor (factorizations)

sumOfSquaresBenchmarks :: Benchmark
sumOfSquaresBenchmarks = bgroup "sumOfSquaresF" (fmap mkBench [1..6])
  where
    mkBench :: Int -> Benchmark
    mkBench i = 
      bench
      ("map sumOfSquares [1..10^" ++ show i ++ "]")
      (nf collapseSumOfSquares (10^i))

collapseSumOfSquares :: Int -> Int
collapseSumOfSquares n =
  take n factorizations
  & map (sum . map (uncurry (+)) . sumOfSquaresF)
  & sum
