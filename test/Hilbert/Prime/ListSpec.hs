module Hilbert.Prime.ListSpec
  ( main
  , spec
  ) where

import Test.Hspec;
import Test.QuickCheck;
import Test.Hspec.QuickCheck;

import Hilbert.Prime.List (primes, primesLessThan)
import Hilbert.Prime.TrialDivision (trialDivision)

main = hspec spec

spec = describe "Prime.List" $ do
         describe "primes" $ do
           it "contains primes and only primes" $ do
             test_primes

         describe "primesLessThan" $ do
           it "matches (takeWhile (<= limit) primes)" $ do
             test_primesLessThan
           it "returns an empty list when given anything smaller than 3" $ do
             (primesLessThan 2) `shouldBe` []
             (primesLessThan 1) `shouldBe` []
             (primesLessThan 0) `shouldBe` [] 
             (primesLessThan (-1)) `shouldBe` [] 
             (primesLessThan (-100)) `shouldBe` [] 

{-
   Supplementary data/functions
-}
numberOfTests = 500
testSizeLimit = 10000

{-
   primes tests
-}
test_primes = (take numberOfTests primes) `shouldMatchList`
              (take numberOfTests $ filter trialDivision [1..])

{-
   primesLessThan tests
-}
test_primesLessThan = (primesLessThan testSizeLimit) `shouldMatchList`
                     (takeWhile (<= testSizeLimit) primes)
