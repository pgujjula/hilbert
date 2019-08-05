module Hilbert.Prime.Factor.TrialDivisionSpec
 ( main
 , spec
 ) where

import Test.Hspec (hspec, describe, Spec, it, shouldBe)
import Hilbert.Prime.Factor.TrialDivision (factor)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "factor" $ do
         smallCases_trialDivision
         bigSmoothNumber_trialDivision

smallCases_trialDivision = 
         it ("correct for all integers up to " ++ (show testLimit)) $ do
           let expected = map (multiplyOut . factor) [1..]
           let actual   = [1..]
           sequence_ $ take testLimit $ zipWith shouldBe actual expected

bigSmoothNumber_trialDivision = 
         it "works on very large numbers with small prime factors" $ do
           let expected = [(2, 100), (3, 50), (5, 50)]
           let actual   = factor (2^100 * 3^50 * 5^50)
           actual `shouldBe` expected

{-
   Supplementary data/functions
-}
multiplyOut :: [(Int, Int)] -> Int
multiplyOut = product . (map (\(p, e) -> p^e))

testLimit :: Int
testLimit = 10000
