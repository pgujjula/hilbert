-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Test.Math.Combinatorics.Binomial (tests) where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.Mod (Mod)
import Data.Type.Natural (SNat, withSNat)
import Math.Combinatorics.Binomial
  ( binomialCoeffs,
    choose,
    chooseModP,
    chooseModP2,
    factorial,
    factorialModP,
    factorialNoPModP,
    factorialNoPModP2,
    factorialRelPrimeModP,
    factorialRelPrimeModP2,
    mkFactorialModP,
    mkFactorialModP2,
    pascalDiagonal,
    permute,
  )
import Math.NumberTheory.Divisor (divides)
import Math.NumberTheory.Prime (primesTo)
import Test.QuickCheck (forAll, (===))
import Test.QuickCheck qualified as QuickCheck (choose)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Util (throwsException)

-- Limit for quickcheck inputs
limit :: Integer
limit = 1000

tests :: TestTree
tests =
  testGroup
    "Math.Combinatorics.Binomial"
    [ factorialTest,
      chooseTest,
      binomialCoeffsTest,
      permuteTest,
      invariantTest,
      pascalDiagonalTest,
      factorialModPTest,
      factorialRelPrimeModPTest,
      factorialNoPModPTest,
      chooseModPTest,
      factorialRelPrimeModP2Test,
      factorialNoPModP2Test,
      chooseModP2Test,
      factorialRelPrimeModP2Test
    ]

factorialTest :: TestTree
factorialTest =
  testGroup
    "factorial tests"
    [ testCase "factorial 0" $ factorial @Int 0 @?= (1 :: Int),
      testCase "factorial 1" $ factorial @Int 1 @?= (1 :: Int),
      testCase "factorial 2" $ factorial @Int 2 @?= (2 :: Int),
      testCase "factorial 10" $ factorial @Int 10 @?= (3628800 :: Int),
      testCase "factorial 50" $
        factorial @Integer 50
          @?= (30414093201713378043612608166064768844377641568960512000000000000 :: Integer)
    ]

chooseTest :: TestTree
chooseTest =
  testGroup
    "choose tests"
    [ testCase "out of bounds is zero" $ do
        choose (-1) 0 @?= (0 :: Int)
        choose 3 4 @?= (0 :: Int)
        choose 3 (-1) @?= (0 :: Int),
      testCase "small input" $
        map (5 `choose`) [(0 :: Int) .. 5] @?= [1 :: Int, 5, 10, 10, 5, 1]
    ]

binomialCoeffsTest :: TestTree
binomialCoeffsTest =
  testGroup
    "binomialCoeffs tests"
    [ testCase "out of bounds throws an error" $ do
        throwsException (binomialCoeffs (-1 :: Int) :: [Int])
        throwsException (binomialCoeffs (-5 :: Int) :: [Int]),
      testCase "matches behavior of choose" $ do
        forM_ [(1 :: Int) .. 10] $ \n -> do
          (binomialCoeffs n :: [Int]) @?= fmap (n `choose`) [(0 :: Int) .. n]
    ]

permuteTest :: TestTree
permuteTest =
  testGroup
    "permute tests"
    [ testCase "out of bounds is zero" $ do
        permute (-1) 0 @?= (0 :: Int)
        permute 3 4 @?= (0 :: Int)
        permute 3 (-1) @?= (0 :: Int),
      testCase "small input" $
        map (5 `permute`) [0 .. 5] @?= [1 :: Int, 5, 20, 60, 120, 120]
    ]

invariantTest :: TestTree
invariantTest =
  testGroup
    "invariant tests"
    [ testProperty "invariant: permute n k == choose n k * factorial k" $
        let gen = do
              n <- QuickCheck.choose (0, limit)
              k <- QuickCheck.choose (0, n)
              return (n, k)
         in forAll gen $ \(n, k) -> permute n k === choose n k * (factorial k :: Integer)
    ]

pascalDiagonalTest :: TestTree
pascalDiagonalTest =
  testGroup
    "pascalDiagonal tests"
    [ testCase "equivalent to map (\\m -> (n+m) `choose` m) [0..]" $
        forM_ [0 .. (10 :: Int)] $ \n ->
          take 10 (pascalDiagonal n :: [Integer])
            @?= take 10 (map (\m -> (n + m) `choose` m) [0 ..])
    ]

factorialModPTest :: TestTree
factorialModPTest =
  testGroup
    "factorialModP tests"
    [ testCase "equivalent to n! (mod p)" $
        forM_ (primesTo 100) $ \p ->
          withSNat (fromIntegral p) $ \(_ :: SNat p) ->
            let cmod = mkFactorialModP @p
             in forM_ [0 .. 100] $ \k ->
                  factorialModP cmod k @?= (factorial k :: Mod p)
    ]

factorialRelPrimeModPTest :: TestTree
factorialRelPrimeModPTest =
  testGroup
    "factorialRelPrimeModP tests"
    [ testCase "works for small n, p" $
        forM_ (map toInteger (primesTo 100)) $ \p ->
          withSNat (fromIntegral p) $ \(_ :: SNat p) ->
            let cmod = mkFactorialModP @p
             in forM_ [1 .. (100 :: Integer)] $ \k ->
                  let expected =
                        fromIntegral $
                          product (filter (not . (p `divides`)) [1 .. k])
                   in factorialRelPrimeModP cmod k @?= expected
    ]

divideOut :: Integral a => a -> a -> a
divideOut n p =
  case quotRem n p of
    (q, 0) -> divideOut q p
    _ -> n

factorialNoPModPTest :: TestTree
factorialNoPModPTest =
  testGroup
    "factorialNoPModP tests"
    [ testCase "works for small n, p" $
        forM_ (map toInteger (primesTo 100)) $ \p ->
          withSNat (fromIntegral p) $ \(_ :: SNat p) ->
            let cmod = mkFactorialModP @p
             in forM_ [1 .. (100 :: Integer)] $ \k ->
                  let expected =
                        fromIntegral
                          (factorial k `divideOut` p)
                   in (k, p, factorialNoPModP cmod k)
                        @?= (k, p, expected)
    ]

chooseModPTest :: TestTree
chooseModPTest =
  testGroup
    "chooseModP tests"
    [ testCase "works for small n, m, p" $
        forM_ (map toInteger (primesTo 100)) $ \p ->
          withSNat (fromIntegral p) $ \(_ :: SNat p) ->
            let fmp = mkFactorialModP @p
             in forM_ [1 .. (30 :: Integer)] $ \n ->
                  forM_ [1 .. (30 :: Integer)] $ \m ->
                    let expected = fromIntegral ((n `choose` m) :: Integer)
                        actual = chooseModP fmp n m
                     in actual @?= expected
    ]

factorialRelPrimeModP2Test :: TestTree
factorialRelPrimeModP2Test =
  testGroup
    "factorialRelPrimeModP2 tests"
    [ testCase "works for small n, p" $
        forM_ (map toInteger (primesTo 100)) $ \p ->
          withSNat (fromIntegral p) $ \(_ :: SNat p) ->
            let fmp2 = mkFactorialModP2 @p
             in forM_ [1 .. (100 :: Integer)] $ \n ->
                  let actual = factorialRelPrimeModP2 fmp2 n
                      expected =
                        fromIntegral $
                          [1 .. n]
                            & filter (not . (p `divides`))
                            & product
                   in (n, p, actual) @?= (n, p, expected)
    ]

factorialNoPModP2Test :: TestTree
factorialNoPModP2Test =
  testGroup
    "factorialNoPModP2 tests"
    [ testCase "works for small n, p" $
        forM_ (map toInteger (primesTo 100)) $ \p ->
          withSNat (fromIntegral p) $ \(_ :: SNat p) ->
            let fmp2 = mkFactorialModP2 @p
             in forM_ [1 .. (100 :: Integer)] $ \n ->
                  let actual = factorialNoPModP2 fmp2 n
                      expected =
                        fromIntegral $
                          [1 .. n]
                            & product
                            & (`divideOut` p)
                   in (n, p, actual) @?= (n, p, expected)
    ]

chooseModP2Test :: TestTree
chooseModP2Test =
  testGroup
    "chooseModP2 tests"
    [ testCase "works for small n, m, p" $
        forM_ (map toInteger (primesTo 10)) $ \p ->
          withSNat (fromIntegral p) $ \(_ :: SNat p) ->
            let fmp = mkFactorialModP2 @p
             in forM_ [1 .. (100 :: Integer)] $ \n ->
                  forM_ [1 .. (100 :: Integer)] $ \m ->
                    let expected = fromIntegral ((n `choose` m) :: Integer)
                        actual = chooseModP2 fmp n m
                     in actual @?= expected
    ]
