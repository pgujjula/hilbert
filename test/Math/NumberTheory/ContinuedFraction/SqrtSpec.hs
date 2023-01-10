{-# LANGUAGE ImportQualifiedPost #-}
module Math.NumberTheory.ContinuedFraction.SqrtSpec (spec) where

import Control.Monad (zipWithM_)
import Math.NumberTheory.ContinuedFraction
  ( ContinuedFraction,
    mkAperiodic,
    mkPeriodic,
  )
import Math.NumberTheory.ContinuedFraction qualified as CF (sqrt)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec =
  describe "sqrt" $
    it "matches with WolframAlpha for sqrt(1) ... sqrt(10)" $
      let testContinuedFrac :: [ContinuedFraction Int]
          testContinuedFrac = map CF.sqrt [1 .. 10]

          correctContinuedFrac :: [ContinuedFraction Int]
          correctContinuedFrac =
            [ mkAperiodic [1],
              mkPeriodic [1] [2],
              mkPeriodic [1] [1, 2],
              mkAperiodic [2],
              mkPeriodic [2] [4],
              mkPeriodic [2] [2, 4],
              mkPeriodic [2] [1, 1, 1, 4],
              mkPeriodic [2] [1, 4],
              mkAperiodic [3],
              mkPeriodic [3] [6]
            ]
       in zipWithM_ shouldBe testContinuedFrac correctContinuedFrac
