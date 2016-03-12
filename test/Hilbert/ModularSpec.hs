module Hilbert.ModularSpec (main, spec) where

import Hilbert.Modular (modPow)
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
positiveGen = choose (1, testSizeLimit)

spec = modifyMaxSuccess (\_ -> numberOfTests) $ 
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
