module Test.Math.NumberTheory.Divisor (tests) where

import Control.Exception (evaluate)
import Control.Monad (forM_, zipWithM_)
import Data.List (sort)
import Data.Maybe (fromJust)
import Math.NumberTheory.Divisor
  ( divides,
    divisorPairs,
    divisorPairsF,
    divisors,
    divisorsF,
    mobius,
    mobiusF,
    mobiuses,
    mobiusesFrom,
    numDivisors,
    numDivisorsF,
    relativelyPrime,
    sumDivisors,
    sumDivisorsF,
    totient,
    totientF,
  )
import Math.NumberTheory.Prime.Factor (factor, factorizations)
import Test.Hspec (anyException, shouldThrow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Divisor"
    [ dividesTest,
      divisorsTest,
      divisorsFTest,
      divisorPairsTest,
      divisorPairsFTest,
      numDivisorsTest,
      numDivisorsFTest,
      sumDivisorsTest,
      sumDivisorsFTest,
      relativelyPrimeTest,
      totientTest,
      totientFTest,
      mobiusTest,
      mobiusFTest,
      mobiusesTest,
      mobiusesFromTest
    ]

limit :: Int
limit = 1000

dividesTest :: TestTree
dividesTest =
  testGroup
    "divides tests"
    [ testCase "0 divides 0" $
        (0 `divides` 0) @?= True,
      testCase "0 doesn't divide anything else" $ do
        (0 `divides` 1) @?= False
        (0 `divides` (-5)) @?= False,
      testCase "small cases" $ do
        (5 `divides` 30) @?= True
        (5 `divides` 31) @?= False
    ]

divisorsTest :: TestTree
divisorsTest =
  testGroup
    "divisors tests"
    [ testCase ("correct for abs n up to " ++ show limit) $
        let naive n = filter (`divides` n) [1 .. abs n]
         in forM_ [(-limit) .. limit] $ \x ->
              sort (divisors x) @?= naive x
    ]

divisorsFTest :: TestTree
divisorsFTest =
  testGroup
    "divisorsF tests"
    [ testCase ("correct for n up to " ++ show limit) $
        let naive n = filter (`divides` n) [1 .. abs n]
         in forM_ [1 .. limit] $ \x ->
              (sort . divisorsF . fromJust . factor $ x) @?= naive x
    ]

divisorPairsTest :: TestTree
divisorPairsTest =
  testGroup
    "divisorPairs tests"
    [ testCase ("correct for abs n up to " ++ show limit) $
        forM_ [(-limit) .. limit] $ \x -> do
          let dps = divisorPairs x
          length dps
            @?= ((length (divisors x) + 1) `div` 2)
          mapM_ (\(a, b) -> a * b @?= abs x) dps
    ]

divisorPairsFTest :: TestTree
divisorPairsFTest =
  testGroup
    "divisorPairsF tests"
    [ testCase ("correct for n up to " ++ show limit) $
        forM_ [1 .. limit] $ \x -> do
          let dps = divisorPairsF . fromJust . factor $ x
          length dps
            @?= ((length (divisors x) + 1) `div` 2)
          mapM_ (\(a, b) -> a * b @?= x) dps
    ]

numDivisorsTest :: TestTree
numDivisorsTest =
  testGroup
    "numDivisors tests"
    [ testCase ("correct for abs n up to " ++ show limit) $
        let naive n = length $ filter (`divides` n) [1 .. abs n]
         in forM_ [(-limit) .. limit] $ \x -> numDivisors x @?= naive x
    ]

numDivisorsFTest :: TestTree
numDivisorsFTest =
  testGroup
    "numDivisorsF tests"
    [ testCase ("correct for n up to " ++ show limit) $
        let naive n = length $ filter (`divides` n) [1 .. abs n]
         in forM_ [1 .. limit] $ \x ->
              (numDivisorsF . fromJust . factor $ x) @?= naive x
    ]

sumDivisorsTest :: TestTree
sumDivisorsTest =
  testGroup
    "sumDivisors tests"
    [ testCase ("correct for abs n up to " ++ show limit) $
        let naive n = sum $ filter (`divides` n) [1 .. abs n]
         in forM_ [(-limit) .. limit] $ \x -> sumDivisors x @?= naive x
    ]

sumDivisorsFTest :: TestTree
sumDivisorsFTest =
  testGroup
    "sumDivisorsF tests"
    [ testCase ("correct for n up to " ++ show limit) $
        let naive n = sum $ filter (`divides` n) [1 .. abs n]
         in forM_ [1 .. limit] $ \x ->
              (sumDivisorsF . fromJust . factor $ x) @?= naive x
    ]

relativelyPrimeTest :: TestTree
relativelyPrimeTest =
  testGroup
    "relativelyPrime tests"
    [ testCase "0 is not relatively prime to 0" $
        relativelyPrime 0 0 @?= False,
      testCase "0 is relatively prime to 1/-1" $ do
        relativelyPrime 0 1 @?= True
        relativelyPrime 0 (-1) @?= True,
      testCase "0 is not relatively to integers with magnitude > 1" $ do
        relativelyPrime 0 2 @?= False
        relativelyPrime 0 (-2) @?= False,
      testCase "1 is relatively prime to 1/-1" $ do
        relativelyPrime 1 1 @?= True
        relativelyPrime 1 (-1) @?= True,
      testCase "3 is relatively prime to 5/-5" $ do
        relativelyPrime 3 5 @?= True
        relativelyPrime 3 (-5) @?= True,
      testCase "-3 is not relatively prime to 6/-6" $ do
        relativelyPrime (-3) 6 @?= False
        relativelyPrime (-3) (-6) @?= False
    ]

totientTest :: TestTree
totientTest =
  testGroup
    "totient tests"
    [ testCase "totient of negative numbers is 0" $
        totient (-1) @?= 0,
      testCase "totient of 0 is 1" $
        totient 0 @?= 1,
      testCase ("correct for up to " ++ show limit) $
        forM_ [1 .. limit] $ \n ->
          totient n @?= length (filter (relativelyPrime n) [1 .. n])
    ]

totientFTest :: TestTree
totientFTest =
  testGroup
    "totientF tests"
    [ testCase ("correct up to " ++ show limit) $
        forM_ [1 .. limit] $ \n ->
          totientF n (fromJust $ factor n)
            @?= length (filter (relativelyPrime n) [1 .. n])
    ]

mobiusTest :: TestTree
mobiusTest =
  testGroup
    "mobius tests"
    [ testCase "mobius of non-positive numbers is undefined" $ do
        evaluate (mobius (-1)) `shouldThrow` anyException
        evaluate (mobius 0) `shouldThrow` anyException,
      testCase "correct up to 10" $ do
        zipWithM_
          (@?=)
          (fmap mobius [1 .. 10])
          [1, -1, -1, 0, -1, 1, -1, 0, 0, 1]
    ]

mobiusFTest :: TestTree
mobiusFTest =
  testGroup
    "mobiusF tests"
    [ testCase "correct up to 10" $ do
        zipWithM_
          (@?=)
          (mobiusF <$> take 10 factorizations)
          [1, -1, -1, 0, -1, 1, -1, 0, 0, 1]
    ]

mobiusesTest :: TestTree
mobiusesTest =
  testGroup
    "mobiuses tests"
    [ testCase "matches mobius up to limit" $ do
        zipWithM_ (@?=) (take limit mobiuses) (map mobius [1 .. limit])
    ]

mobiusesFromTest :: TestTree
mobiusesFromTest =
  testGroup
    "mobiusesFrom tests"
    [ testCase "correct up to limit, for many start points" $
        forM_ [1 .. 30] $ \i ->
          take limit (mobiusesFrom i)
            @?= take limit (drop (i - 1) mobiuses)
    ]
