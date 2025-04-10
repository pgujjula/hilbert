-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module Test.Math.NumberTheory.Power (tests) where

import Data.Maybe (fromJust, isJust)
import Math.NumberTheory.Power
  ( cube,
    cubes,
    integralLogBase,
    square,
    squares,
  )
import Test.QuickCheck
  ( Gen,
    choose,
    forAll,
    oneof,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Power"
    [squareTest, squaresTest, cubeTest, cubesTest, integralLogBaseTest]

listLimit :: Int
listLimit = 100

compareList :: (Eq a, Show a) => [a] -> [a] -> Assertion
compareList xs ys = take listLimit xs @?= take listLimit ys

squareTest :: TestTree
squareTest =
  testGroup
    "square tests"
    [ testCase ("first " ++ show listLimit ++ " correct") $ do
        compareList (fmap square [(0 :: Int) ..]) (map (^ 2) [0 ..])
        compareList (fmap square [(0 :: Double) ..]) (map (^ 2) [0 ..])
    ]

squaresTest :: TestTree
squaresTest =
  testGroup
    "squares tests"
    [ testCase ("first " ++ show listLimit ++ " correct") $
        compareList squares (fmap (^ 2) [0 ..])
    ]

cubeTest :: TestTree
cubeTest =
  testGroup
    "cube tests"
    [ testCase ("first " ++ show listLimit ++ " correct") $
        compareList (fmap cube [0 ..]) (fmap (^ 3) [0 ..])
    ]

cubesTest :: TestTree
cubesTest =
  testGroup
    "cubes tests"
    [ testCase ("first " ++ show listLimit ++ " correct") $
        take listLimit cubes @?= take listLimit (map (^ 3) [0 ..])
    ]

integralLogBaseTest :: TestTree
integralLogBaseTest =
  testGroup
    "integralLogBase tests"
    [ testCase "negative base yields Nothing" $ do
        integralLogBase (-2) 3 @?= Nothing
        integralLogBase (-5) 3 @?= Nothing,
      testCase "zero base yields Nothing" $
        integralLogBase 0 3 @?= Nothing,
      testCase "base of 1 yields Nothing" $
        integralLogBase 1 3 @?= Nothing,
      testCase "negative input yields Nothing" $ do
        integralLogBase 3 (-5) @?= Nothing
        integralLogBase 3 (-2) @?= Nothing,
      testCase "zero input yields Nothing" $
        integralLogBase 3 0 @?= Nothing,
      testProperty "valid for general input" $ do
        let baseGen :: Gen Integer
            baseGen = choose (2, 10)

        let inputGen :: Gen Integer
            inputGen = oneof [choose (1, 10), choose (1, 1000), choose (1, 10 ^ 100)]

        forAll ((,) <$> baseGen <*> inputGen) $ \(b, n) -> do
          let maybeE = integralLogBase b n
          let e = fromJust maybeE
          isJust maybeE && (b ^ e <= n) && (b ^ (e + 1) > n)
    ]
