module Test.Math.NumberTheory.Prime (tests) where

import Math.NumberTheory.Prime
  ( composites,
    compositesTo,
    isPrime,
    primes,
    primesFromTo,
    primesTo,
    primesChimera,
    isPrimeChimera
  )
import Math.NumberTheory.Roots (integerSquareRoot)
import Data.Chimera qualified as Chimera
import Data.List (elemIndices)
import Test.QuickCheck (choose, forAll, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Prime"
    [ isPrimeTest,
      primesTest,
      primesToTest,
      primesFromToTest,
      compositesTest,
      compositesToTest,
      primesChimeraTest,
      isPrimeChimeraTest
    ]

naiveIsPrime :: Integral a => a -> Bool
naiveIsPrime n =
  n >= 2 && not (any (\d -> n `rem` d == 0) [2 .. integerSquareRoot n])

isPrimeTest :: TestTree
isPrimeTest =
  testGroup
    "isPrime tests"
    [ testCase "negative numbers not prime" $ do
        isPrime (-1) @?= False
        isPrime (-2) @?= False
        -- (-1995) is prime when cast to a Word, so this checks that we are
        -- checking for negative numbers before casting.
        isPrime (-1995) @?= False,
      testCase "0 not prime" $
        isPrime 0 @?= False,
      testCase "1 not prime" $
        isPrime 1 @?= False,
      testCase "primes up to 1000 correct" $
        filter isPrime [1 .. 1000] @?= filter naiveIsPrime [1 .. 1000],
      testProperty "works on arbitrary Ints" $
        forAll (choose (1 :: Int, 100000)) $
          \x -> isPrime x === naiveIsPrime x
    ]

primesTest :: TestTree
primesTest =
  testCase "correct for primes up to 10000" $
    takeWhile (<= 10000) primes
      @?= filter naiveIsPrime [1 .. 10000]

primesToTest :: TestTree
primesToTest =
  testGroup
    "primesTo tests"
    [ testCase "correct for degenerate cases" $ do
        primesTo (-1) @?= []
        primesTo 0 @?= []
        primesTo 1 @?= []
        primesTo 2 @?= [2]
        primesTo 3 @?= [2, 3],
      testCase "correct for primes up to 10000" $
        primesTo 10000
          @?= filter naiveIsPrime [1 .. 10000],
      testCase "is inclusive" $
        primesTo 97
          @?= filter naiveIsPrime [1 .. 97]
    ]

primesFromToTest :: TestTree
primesFromToTest =
  testGroup
    "primesFromTo tests"
    [ testCase "correct for degenerate cases" $ do
        primesFromTo 5 3 @?= []
        primesFromTo 0 1 @?= []
        primesFromTo 0 2 @?= [2]
        primesFromTo 0 3 @?= [2, 3],
      testProperty "correct for random starts and stops" $ do
        let limit = 10000
        forAll (choose (1, limit)) $ \lower ->
          forAll (choose (lower, limit)) $ \upper ->
            primesFromTo lower upper === filter isPrime [lower .. upper]
    ]

compositesTest :: TestTree
compositesTest =
  testGroup
    "composites tests"
    [ testCase "correct for composities up to 10000" $
        takeWhile (<= 10000) composites @?= filter (not . naiveIsPrime) [2 .. 10000]
    ]

compositesToTest :: TestTree
compositesToTest =
  testGroup
    "compositesTo tests"
    [ testCase "correct for composities up to 10000" $
        compositesTo 10000 @?= filter (not . naiveIsPrime) [2 .. 10000]
    ]

primesChimeraTest :: TestTree
primesChimeraTest =
  testGroup
    "primeChimera tests"
    [ testCase "correct for primes up to 10000" $
        takeWhile (<= 10000) (Chimera.toList primesChimera) @?= map fromIntegral (primesTo 10000)
    ]

isPrimeChimeraTest :: TestTree
isPrimeChimeraTest =
  testGroup
    "isPrimeChimera tests"
    [ testCase "correct up to 10000" $
      elemIndices 1 (take 10001 (Chimera.toList isPrimeChimera))
        @?= primesTo 10000
    ]
