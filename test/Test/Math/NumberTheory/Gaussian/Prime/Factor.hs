module Test.Math.NumberTheory.Gaussian.Prime.Factor (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Gaussian.Prime.Factor"
    [factorTest, factorizationTest]

factorTest :: TestTree
factorTest = ignoreTest $ testCase "factor tests" (pure ())

factorizationTest :: TestTree
factorizationTest = ignoreTest $ testCase "factorization tests" (pure ())
