-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE DataKinds #-}

module Test.Math.NumberTheory.QuadraticExtension (tests) where

import Control.Monad (forM_, when)
import Data.Ratio (Ratio, (%))
import Data.Type.Natural (SNat, withSNat)
import Math.NumberTheory.Divisor (mobius)
import Math.NumberTheory.QuadraticExtension (QuadraticExtension (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (sqrt)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.QuadraticExtension"
    [ showTest,
      ordTest,
      numTest,
      fractionalTest,
      semiringTest,
      ringTest
    ]

showTest :: TestTree
showTest = ignoreTest $ testCase "showTest" (pure ())

ordTest :: TestTree
ordTest = ignoreTest $ testCase "ordTest" (pure ())

numTest :: TestTree
numTest = ignoreTest $ testCase "numTest" (pure ())

fractionalTest :: TestTree
fractionalTest =
  testGroup
    "fractionalTest"
    [ testCase "x * recip x == 1" $
        let squarefreeTo :: Int -> [Int]
            squarefreeTo n = filter (\i -> mobius i /= 0) [1 .. n]
         in forM_ (squarefreeTo 10) $ \d ->
              withSNat (fromIntegral d) $ \(_ :: SNat d) ->
                forM_ [-20 .. 20] $ \(a :: Int) ->
                  forM_ [-20 .. 20] $ \(b :: Int) ->
                    when (a /= 0 || b /= 0) $
                      let x :: QuadraticExtension 3 (Ratio Int)
                          x = QuadraticExtension (a % 1) (b % 1)
                       in (x * recip x) @?= 1,
      testCase "fromRational (1 % 2) == QuadraticExtension @3 (1 % 2) 0" $
        fromRational (1 % 2) @?= QuadraticExtension @3 (1 % 2) 0
    ]

semiringTest :: TestTree
semiringTest = ignoreTest $ testCase "semiringTest" (pure ())

ringTest :: TestTree
ringTest = ignoreTest $ testCase "ringTest" (pure ())
