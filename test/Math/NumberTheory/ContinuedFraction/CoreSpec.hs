module Math.NumberTheory.ContinuedFraction.CoreSpec
  ( spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow, anyException, shouldStartWith)
import Test.QuickCheck (property)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.HUnit.Lang (assertFailure)
import Control.Exception (evaluate)
import Data.Ratio ((%))

import Math.NumberTheory.ContinuedFraction
  ( mkPeriodic
  , mkAperiodic
  , repeatingPart
  , nonRepeatingPart
  , toList
  , isPeriodic
  , convergent
  )

-- Parameters
numberOfTests :: Int
numberOfTests = 100

-- Tests
spec :: Spec
spec = modifyMaxSuccess (\_ -> numberOfTests) $ do
           describe "mkPeriodic" $ do
             it "mkPeriodic _ [] throws an error" $ property $
               \xs -> evaluate (mkPeriodic (xs :: [Int]) [])
                        `shouldThrow`
                      anyException

           describe "repeatingPart" $ do
             it "returns Nothing for aperiodic continued fraction" $ do
               repeatingPart (mkAperiodic [1, 2, 3]) `shouldBe` Nothing
             it "returns the right part for periodic continued fraction" $ do
               repeatingPart (mkPeriodic [1, 2] [3, 4])
                 `shouldBe` (Just [3, 4])

           describe "nonRepeatingPart" $ do
             it ("returns the non-repeating part for aperiodic and periodic " 
                     ++ "continued fractions") $ do
               nonRepeatingPart (mkPeriodic [1, 2] [3, 4]) `shouldBe` [1, 2]
               nonRepeatingPart (mkAperiodic [1, 2, 3]) `shouldBe` [1, 2, 3]
           
           describe "toList" $ do
             it "works for aperiodic continued fractions" $ do
               toList (mkAperiodic [1, 2, 3]) `shouldBe` [1, 2, 3]
             it "works for periodic continued fractions" $ do
               toList (mkPeriodic [1, 2] [3, 4])
                 `shouldStartWith` [1, 2, 3, 4, 3, 4]

           describe "isPeriodic" $ do
             it "classifies periodic and aperiodic continued fractions" $ do
               isPeriodic (mkPeriodic [1, 2] [3, 4]) `shouldBe` True
               isPeriodic (mkAperiodic [1, 2, 3]) `shouldBe` False

           describe "convergent" $ do
             it "approximates e well" $ do 
               let e = mkAperiodic $ (2:) $ concat $ map (\x -> [1, x, 1]) [2, 4..]
               let shouldBeCloseTo x y = if abs (x - y) < 10**(-20)
                                         then return ()
                                         else assertFailure $ (show x)
                                                ++ " not close enough to " ++ (show y)
               (fromRational $ convergent e 100) `shouldBeCloseTo` 2.718281828459045
             it "works for periodic fractions" $ do
               convergent (mkPeriodic [1, 2] [3, 4]) 0 `shouldBe` (0 % 1)
               convergent (mkPeriodic [1, 2] [3, 4]) 4 `shouldBe` (43 % 30)
               convergent (mkPeriodic [1, 2] [3, 4]) 6 `shouldBe` (599 % 418)
               convergent (mkPeriodic [] [3, 4]) 3 `shouldBe` (42 % 13)
             it "works for aperiodic fractions" $ do
               convergent (mkAperiodic [1, 2, 3]) 0 `shouldBe` (0 % 1)
               convergent (mkAperiodic [1, 2, 3]) 2 `shouldBe` (3 % 2)
               convergent (mkAperiodic [1, 2, 3]) 4 `shouldBe` (10 % 7)
               convergent (mkAperiodic []) 5 `shouldBe` (0 % 1)

