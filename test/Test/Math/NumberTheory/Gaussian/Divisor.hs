module Test.Math.NumberTheory.Gaussian.Divisor (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Gaussian.Divisor"
    [ dividesTest,
      relativelyPrimeTest,
      divisorsTest,
      divisorsFTest,
      divisorPairsTest,
      divisorPairsFTest,
      numDivisorsTest,
      numDivisorsFTest
    ]

dividesTest :: TestTree
dividesTest = ignoreTest $ testCase "divides tests" (pure ())

relativelyPrimeTest :: TestTree
relativelyPrimeTest = ignoreTest $ testCase "relativelyPrime tests" (pure ())

divisorsTest :: TestTree
divisorsTest = ignoreTest $ testCase "divisors tests" (pure ())

divisorsFTest :: TestTree
divisorsFTest = ignoreTest $ testCase "divisorsF tests" (pure ())

divisorPairsTest :: TestTree
divisorPairsTest = ignoreTest $ testCase "divisorPairs tests" (pure ())

divisorPairsFTest :: TestTree
divisorPairsFTest = ignoreTest $ testCase "divisorPairsF tests" (pure ())

numDivisorsTest :: TestTree
numDivisorsTest = ignoreTest $ testCase "numDivisors tests" (pure ())

numDivisorsFTest :: TestTree
numDivisorsFTest = ignoreTest $ testCase "numDivisorsF tests" (pure ())
