module Hilbert.ContinuedFraction.SqrtSpec
  ( main
  , spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Hilbert.ContinuedFraction as CF
  ( ContinuedFraction
  , mkPeriodic
  , mkAperiodic
  , sqrt
  )

main :: IO ()
main = hspec spec

spec = modifyMaxSuccess (\_ -> numberOfTests) $ 
         describe "ContinuedFraction.sqrt" $ do
           describe "sqrt" $ do
             it "matches with WolframAlpha for sqrt(1) ... sqrt(10)" $ do
               test_sqrt

{-
   Supplementary data/functions
-}
numberOfTests :: Int
numberOfTests = 100

{-
   sqrt tests
-}
-- Ensure that the Hilbert sqrt function returns the same thing as Wolfram Alpha
test_sqrt = sequence_ $ zipWith shouldBe testContinuedFrac correctContinuedFrac
  where -- The continued fractions of sqrt(1), sqrt(2), ..., sqrt(10)
        -- courtesy of Wolfram Alpha.
        correctContinuedFrac :: [ContinuedFraction Int]
        correctContinuedFrac =
           [ mkAperiodic [1]
           , mkPeriodic  [1] [2]
           , mkPeriodic  [1] [1, 2]
           , mkAperiodic [2]
           , mkPeriodic  [2] [4]
           , mkPeriodic  [2] [2, 4]
           , mkPeriodic  [2] [1, 1, 1, 4]
           , mkPeriodic  [2] [1, 4]
           , mkAperiodic [3]
           , mkPeriodic  [3] [6]
           ]

        -- The continued fractions returned by sqrt
        testContinuedFrac = map CF.sqrt [1..10]
