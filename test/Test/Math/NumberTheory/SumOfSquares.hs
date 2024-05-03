module Test.Math.NumberTheory.SumOfSquares (tests) where

import Control.Monad (forM_, guard)
import Data.List (genericLength, sort)
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.SumOfSquares
  ( numSumOfSquares,
    numSumOfSquaresLE,
    sumOfSquares,
    sumOfSquaresUnique,
  )
import Math.NumberTheory.SumOfSquares.Internal
  ( sumOfSquaresNaive,
    sumOfSquaresUniqueNaive,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.SumOfSquares"
    [ sumOfSquaresNaiveTest,
      sumOfSquaresUniqueNaiveTest,
      sumOfSquaresTest,
      sumOfSquaresUniqueTest,
      numSumOfSquaresTest,
      numSumOfSquaresLETest
    ]

sumOfSquaresVeryNaive :: (Integral a) => a -> [(a, a)]
sumOfSquaresVeryNaive n | n < 0 = []
sumOfSquaresVeryNaive n = do
  let sq = integerSquareRoot n
  a <- [-sq .. sq]
  b <- [-sq .. sq]
  guard (a * a + b * b == n)
  pure (a, b)

sumOfSquaresUniqueVeryNaive :: (Integral a) => a -> [(a, a)]
sumOfSquaresUniqueVeryNaive n =
  filter (\(a, b) -> 0 <= a && a <= b) $ sumOfSquaresVeryNaive n

sumOfSquaresNaiveTest :: TestTree
sumOfSquaresNaiveTest =
  testGroup
    "sumOfSquaresNaive"
    [ testCase "empty list for negative inputs" $
        forM_ [(-10 :: Int) .. (-1 :: Int)] $ \i ->
          sumOfSquaresNaive i @?= [],
      testCase "correct for n in [0..100]" $
        forM_ [(0 :: Int) .. 100] $ \n ->
          sort (sumOfSquaresNaive n)
            @?= sort (sumOfSquaresVeryNaive n)
    ]

sumOfSquaresUniqueNaiveTest :: TestTree
sumOfSquaresUniqueNaiveTest =
  testGroup
    "sumOfSquaresUniqueNaive"
    [ testCase "empty list for negative inputs" $
        forM_ [(-10 :: Int) .. (-1 :: Int)] $ \i ->
          sumOfSquaresUniqueNaive i @?= [],
      testCase "correct for n in [0..100]" $
        forM_ [(0 :: Int) .. 100] $ \n ->
          sort (sumOfSquaresUniqueNaive n)
            @?= sort (sumOfSquaresUniqueVeryNaive n)
    ]

sumOfSquaresTest :: TestTree
sumOfSquaresTest =
  testGroup
    "sumOfSquares"
    [ testCase "empty list for negative inputs" $
        forM_ [(-10 :: Int) .. (-1 :: Int)] $ \i ->
          sumOfSquares i @?= [],
      testCase "correct for n in [0..1000]" $
        forM_ [(0 :: Int) .. 1000] $ \n ->
          sort (sumOfSquares n)
            @?= sort (sumOfSquaresNaive n)
    ]

sumOfSquaresUniqueTest :: TestTree
sumOfSquaresUniqueTest =
  testGroup
    "sumOfSquaresUnique"
    [ testCase "empty list for negative inputs" $
        forM_ [(-10 :: Int) .. (-1 :: Int)] $ \i ->
          sumOfSquaresUnique i @?= [],
      testCase "correct for n in [0..10000]" $
        forM_ [(0 :: Int) .. 10000] $ \n ->
          sort (sumOfSquaresUnique n)
            @?= sort (sumOfSquaresUniqueNaive n)
    ]

numSumOfSquaresTest :: TestTree
numSumOfSquaresTest =
  testGroup
    "numSumOfSquares"
    [ testCase "0 for negative inputs" $
        forM_ [(-10 :: Int) .. (-1 :: Int)] $ \i ->
          numSumOfSquares i @?= 0,
      testCase "correct for n in [0..10000]" $
        forM_ [(0 :: Int) .. 10000] $ \n ->
          numSumOfSquares n @?= length (sumOfSquares n)
    ]

numSumOfSquaresLETest :: TestTree
numSumOfSquaresLETest =
  testGroup
    "numSumOfSquaresLE"
    [ testCase "0 for negative inputs" $
        forM_ [(-10 :: Int) .. (-1 :: Int)] $ \i ->
          numSumOfSquaresLE i @?= 0,
      testCase "correct for n in [0..200]" $
        forM_ [(0 :: Int) .. 200] $ \n ->
          assertEqual
            (show n)
            (numSumOfSquaresLENaive n)
            (numSumOfSquaresLE n)
    ]

numSumOfSquaresLENaive :: (Integral a) => a -> a
numSumOfSquaresLENaive n = genericLength $ do
  a <- [0 .. integerSquareRoot n]
  b <- [0 .. integerSquareRoot n]
  guard (a * a + b * b <= n)
  pure (a, b)
