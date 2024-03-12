module Test.Math.NumberTheory.Gaussian.Prime (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.ExpectedFailure (ignoreTest)

tests :: TestTree
tests = testGroup "Math.NumberTheory.Gaussian.Prime" [isPrimeTest, primesTest]

isPrimeTest :: TestTree
isPrimeTest = ignoreTest $ testCase "isPrime tests" (pure ())

primesTest :: TestTree
primesTest = ignoreTest $ testCase "primes tests" (pure ())
