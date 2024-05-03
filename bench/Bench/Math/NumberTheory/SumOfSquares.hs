module Bench.Math.NumberTheory.SumOfSquares
  ( sumOfSquaresBenchmarks,
    numSumOfSquaresLEBenchmarks,
    numSumOfSquaresLERefBenchmarks,
  )
where

import Data.Function ((&))
import Math.NumberTheory.Prime.Factor (factorizations)
import Math.NumberTheory.SumOfSquares (numSumOfSquaresLE, sumOfSquaresF)
import Math.NumberTheory.SumOfSquares.Internal (numSumOfSquaresLERef)
import Test.Tasty.Bench

numSumOfSquaresLEBenchmarks :: Benchmark
numSumOfSquaresLEBenchmarks =
  bgroup "numSumOfSquaresLE" (fmap mkBench [1 .. 8])
  where
    mkBench :: Int -> Benchmark
    mkBench i =
      bench
        ("map numSumOfSquaresLE [1..10^" ++ show i ++ "]")
        (nf numSumOfSquaresLE (10 ^ i :: Int))

numSumOfSquaresLERefBenchmarks :: Benchmark
numSumOfSquaresLERefBenchmarks =
  bgroup "numSumOfSquaresLERef" (fmap mkBench [1 .. 8])
  where
    mkBench :: Int -> Benchmark
    mkBench i =
      bench
        ("map numSumOfSquaresLERef [1..10^" ++ show i ++ "]")
        (nf numSumOfSquaresLERef (10 ^ i :: Int))

sumOfSquaresBenchmarks :: Benchmark
sumOfSquaresBenchmarks = bgroup "sumOfSquaresF" (fmap mkBench [1 .. 6])
  where
    mkBench :: Int -> Benchmark
    mkBench i =
      bench
        ("map sumOfSquares [1..10^" ++ show i ++ "]")
        (nf collapseSumOfSquares (10 ^ i))

collapseSumOfSquares :: Int -> Int
collapseSumOfSquares n =
  take n factorizations
    & map (sum . map (uncurry (+)) . sumOfSquaresF)
    & sum
