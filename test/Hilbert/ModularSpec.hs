module Hilbert.ModularSpec (main, spec) where

import Hilbert.Modular (modPow)
import Data.Int (Int8)
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import System.Random

main :: IO ()
main = hspec spec

-- The maximum size of any argument in these tests
testSizeLimit = 10000 :: Integer

-- The number of tests to conduct of each type
numberOfTests = 1000 :: Int

-- Generates only positive test cases
positiveGen :: Gen Integer
positiveGen = choose (1, testSizeLimit)

smallGen :: Gen Integer
smallGen = choose (1, fromIntegral (maxBound :: Int8))

spec :: SpecWith()
spec = modifyMaxSuccess (\_ -> numberOfTests) $ 
  describe "Modular" $ do 
  describe "modPow" $ do
    it "modPow 0 0 m == 1 for all m > 0" $ do
      forAll positiveGen $ \m -> 
        (modPow 0 0 m) == 1

    it "modPow a 0 m == 1 for all a, m > 0" $
      forAll positiveGen $ \a -> 
      forAll positiveGen $ \m -> 
        (modPow a 0 m) == 1

    it "modPow 0 b m == 0 for all b, m > 0" $
      forAll positiveGen $ \b ->
      forAll positiveGen $ \m ->
        (modPow 0 b m) == 0

    it "modPow a b m == (a^b) (mod m) if a, b, m > 0" $
      forAll positiveGen $ \a -> 
      forAll positiveGen $ \b -> 
      forAll positiveGen $ \m -> 
        (modPow a b m) == ((a^b) `mod` m)

    it "doesn't overflow if we use a fixed-precision Integral type" $ 
      forAll smallGen $ \a -> 
      forAll smallGen $ \b -> 
      forAll smallGen $ \m -> 
        (toInteger (modPow ((fromIntegral a) :: Int8)
                           ((fromIntegral b) :: Int8)
                           ((fromIntegral m) :: Int8))) == (modPow a b m)
