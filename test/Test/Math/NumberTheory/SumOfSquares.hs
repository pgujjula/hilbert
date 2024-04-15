module Test.Math.NumberTheory.SumOfSquares (tests) where

import Math.NumberTheory.SumOfSquares (numSumOfSquaresF, numSumOfSquaresOfSquareF, sumOfSquaresF, sumOfSquaresOfSquareF)
import Test.QuickCheck (Gen, choose, forAll)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, (.&&.), (===))

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.SumOfSquares"
    [ sumOfSquaresFTest,
      numSumOfSquaresFTest,
      sumOfSquaresOfSquareFTest,
      numSumOfSquaresOfSquareFTest
    ]

sumOfSquaresFTest :: TestTree
sumOfSquaresFTest = testGroup "sumOfSquaresF" []

numSumOfSquaresFTest :: TestTree
numSumOfSquaresFTest = testGroup "numSumOfSquaresF" []

sumOfSquaresOfSquareFTest :: TestTree
sumOfSquaresOfSquareFTest = testGroup "sumOfSquaresOfSquareF" []

numSumOfSquaresOfSquareFTest :: TestTree
numSumOfSquaresOfSquareFTest = testGroup "numSumOfSquaresOfSquareF" []
