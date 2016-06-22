module Hilbert.Prime.TrialDivisionSpec
  ( main
  , spec
  ) where

import Hilbert.Prime.TrialDivision (trialDivision)
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Data.Int (Int8)

main = hspec spec

spec = modifyMaxSize (\_ -> 1000000) $ 
         modifyMaxSuccess (\_ -> 200) $
           describe "Prime.TrialDivision" $ do
             smallCases_trialDivision
             definitionCheck_trialDivision
             fixedPrecision_trialDivision

{- 
   Supplementary functions/data
-}
-- A primality test using the most basic definition: A number is prime if it
-- has exactly two divisors.
naive :: (Integral a) => a -> Bool
naive n = (== 2) $ length $
          filter ((== 0) . (n `rem` ))
                 [1..n]

primesLessThan100 = [ 2,  3,  5,  7, 11,
                     13, 17, 19, 23, 29, 
                     31, 37, 41, 43, 47,
                     53, 59, 61, 67, 71,
                     73, 79, 83, 89, 97]

integerGen :: Gen Integer
integerGen = choose (1, 300000)
              
{-
   trialDivision tests
-}
smallCases_trialDivision = 
  it "works on small numbers" $ do
    (filter trialDivision [1..100]) `shouldBe` primesLessThan100
  
definitionCheck_trialDivision = 
  it "conforms to the definition of primality" $ do
    forAll integerGen $ \x ->
      (trialDivision x) `shouldBe` (naive x)
 
fixedPrecision_trialDivision = 
  it "has no overflow issues" $ do
    let bound = maxBound :: Int8
    (filter trialDivision [1..bound]) `shouldBe` (filter naive [1..bound])
