module Math.NumberTheory.ContinuedFraction.CoreSpec
  ( spec
  ) where

import Control.Exception                   (evaluate)
import Data.Ratio                          ((%))
import Test.Hspec                          (Spec, anyException, describe, it,
                                            shouldBe, shouldStartWith,
                                            shouldThrow)
import Test.HUnit.Lang                     (assertFailure)

import Math.NumberTheory.ContinuedFraction (convergent, isPeriodic, mkAperiodic,
                                            mkPeriodic, nonRepeatingPart,
                                            repeatingPart, toList)

spec :: Spec
spec = do
    describe "mkPeriodic" $
        it "mkPeriodic _ [] throws an error" $ 
            evaluate (mkPeriodic ([1, 2, 3] :: [Int]) [])
                     `shouldThrow` anyException

    describe "repeatingPart" $ do
        it "aperiodic continued fraction" $ do
            repeatingPart (mkAperiodic [1, 2, 3]) `shouldBe` Nothing
        it "periodic continued fraction" $ do
            repeatingPart (mkPeriodic [1, 2] [3, 4])
                `shouldBe` (Just [3, 4])

    describe "nonRepeatingPart" $ do
        it "periodic continued fraction" $
            nonRepeatingPart (mkPeriodic [1, 2] [3, 4]) `shouldBe` [1, 2]
        it "aperiodic continued fraction" $
            nonRepeatingPart (mkAperiodic [1, 2, 3]) `shouldBe` [1, 2, 3]

    describe "toList" $ do
        it "aperiodic continued fractions" $
            toList (mkAperiodic [1, 2, 3]) `shouldBe` [1, 2, 3]
        it "periodic continued fractions" $
            toList (mkPeriodic [1, 2] [3, 4])
                `shouldStartWith` [1, 2, 3, 4, 3, 4, 3, 4]

    describe "isPeriodic" $ do
        it "periodic continued fractions" $
            isPeriodic (mkPeriodic [1, 2] [3, 4]) `shouldBe` True
        it "aperiodic continued fractions" $
            isPeriodic (mkAperiodic [1, 2, 3]) `shouldBe` False

    describe "convergent" $ do
        it "approximates e well" $ do
            let e = mkAperiodic $ (2:) $ concat $ map (\x -> [1, x, 1]) [2, 4..]
            let shouldBeCloseTo x y =
                    if abs (x - y) < 10**(-20)
                    then return ()
                    else assertFailure $
                             show x ++ " not close enough to " ++ show y
            (fromRational $ convergent e 100) `shouldBeCloseTo` 2.718281828459045
        it "periodic fractions" $ do
            convergent (mkPeriodic [1, 2] [3, 4]) 0 `shouldBe` (0 % 1)
            convergent (mkPeriodic [1, 2] [3, 4]) 4 `shouldBe` (43 % 30)
            convergent (mkPeriodic [1, 2] [3, 4]) 6 `shouldBe` (599 % 418)
            convergent (mkPeriodic [] [3, 4]) 3 `shouldBe` (42 % 13)
        it "aperiodic fractions" $ do
            convergent (mkAperiodic [1, 2, 3]) 0 `shouldBe` (0 % 1)
            convergent (mkAperiodic [1, 2, 3]) 2 `shouldBe` (3 % 2)
            convergent (mkAperiodic [1, 2, 3]) 4 `shouldBe` (10 % 7)
            convergent (mkAperiodic []) 5 `shouldBe` (0 % 1)
