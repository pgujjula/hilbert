module Hilbert.Prime.FactorSpec
  ( main
  , spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Hilbert.Prime.Factor (factor)

main :: IO ()
main = hspec spec

spec = modifyMaxSize (\_ -> numberOfTests) $ do
         describe "factor" $ do
           it ("correct for all positive integers less than " 
               ++ (show testSizeLimit)) $ do
             sequence_ $ zipWith shouldBe
                            (map (multiplyOut . factor) [1..testSizeLimit])
                            [1..testSizeLimit]

{-
  Supplementary data/functions
-}
multiplyOut :: (Integral a) => [(a, a)] -> a
multiplyOut = product . (map (\(a, b) -> a^b))

numberOfTests :: Int
numberOfTests = 500

testSizeLimit :: Int
testSizeLimit = 10000
