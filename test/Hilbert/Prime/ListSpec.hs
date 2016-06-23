module Hilbert.Prime.ListSpec
  ( main
  , spec
  ) where

import Test.Hspec;
import Test.QuickCheck;
import Test.Hspec.QuickCheck;

import Hilbert.Prime.List (primes)
import Hilbert.Prime.TrialDivision (trialDivision)

main = hspec spec

spec = describe "Prime.List" $ 
         describe "primes" $ 
           it "contains primes and only primes" $ do
             test_primes

{-
   Supplementary data/functions
-}
numberOfTests = 500

{-
   primes tests
-}
test_primes = (take numberOfTests primes) `shouldMatchList`
              (take numberOfTests $ filter trialDivision [1..])
