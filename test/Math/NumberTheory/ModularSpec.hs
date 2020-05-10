module Math.NumberTheory.ModularSpec
  ( main
  , spec
  ) where

import Data.Int (Int8)
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import System.Random

import Math.NumberTheory.Modular (modPow)

main :: IO ()
main = hspec spec

spec :: SpecWith()
spec = modifyMaxSuccess (\_ -> numberOfTests) $ 
    describe "modPow" $ do
      edgeCase1_modPow
      edgeCase2_modPow
      edgeCase3_modPow
      fixedPrecision_modPow
      generalCase_modPow

{-
   Supplementary data
-}
-- The maximum size of any argument in these tests
testSizeLimit = 10000 :: Integer

-- The number of tests to conduct of each type
numberOfTests = 1000 :: Int

-- Generates only positive test cases
positiveGen :: Gen Integer
positiveGen = choose (1, testSizeLimit)

smallGen :: Gen Integer
smallGen = choose (1, fromIntegral (maxBound :: Int8))

{-
   modPow functions
-}
edgeCase1_modPow =
    it "modPow 0 0 m == 1 for all m > 0" $ do
      forAll positiveGen $ \m -> 
        (modPow 0 0 m) == 1

edgeCase2_modPow = 
    it "modPow a 0 m == 1 for all a, m > 0" $
      forAll positiveGen $ \a -> 
      forAll positiveGen $ \m -> 
        (modPow a 0 m) == 1

edgeCase3_modPow = 
    it "modPow 0 b m == 0 for all b, m > 0" $
      forAll positiveGen $ \b ->
      forAll positiveGen $ \m ->
        (modPow 0 b m) == 0

fixedPrecision_modPow = 
    it "works on fixed-precision integers" $ 
      forAll smallGen $ \a -> 
      forAll smallGen $ \b -> 
      forAll smallGen $ \m -> 
        (toInteger (modPow ((fromIntegral a) :: Int8)
                           ((fromIntegral b) :: Int8)
                           ((fromIntegral m) :: Int8))) == (modPow a b m)

generalCase_modPow = 
    it "works on arbitrary precision integers" $
      forAll positiveGen $ \a -> 
      forAll positiveGen $ \b -> 
      forAll positiveGen $ \m -> 
        (modPow a b m) == ((a^b) `mod` m)
