module Test.Math.NumberTheory.SumOfSquares (tests) where

import Control.Monad (forM_, guard)
import Data.List (sort)
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.SumOfSquares ()
import Math.NumberTheory.SumOfSquares.Internal
  ( sumOfSquaresNaive,
    sumOfSquaresUniqueNaive,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.SumOfSquares"
    [ sumOfSquaresNaiveTest,
      sumOfSquaresUniqueNaiveTest,
      sumOfSquaresFTest,
      sumOfSquaresUniqueFTest,
      numSumOfSquaresFTest
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

sumOfSquaresFTest :: TestTree
sumOfSquaresFTest = ignoreTest $ testGroup "sumOfSquaresF" []

sumOfSquaresUniqueFTest :: TestTree
sumOfSquaresUniqueFTest = ignoreTest $ testGroup "sumOfSquaresUniqueF" []

numSumOfSquaresFTest :: TestTree
numSumOfSquaresFTest = ignoreTest $ testGroup "numSumOfSquaresF" []
