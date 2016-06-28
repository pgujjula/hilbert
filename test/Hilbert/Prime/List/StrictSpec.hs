module Hilbert.Prime.List.StrictSpec
  ( main
  , spec
  ) where

import Test.Hspec (hspec, Spec, describe, it, shouldBe)
import Hilbert.Prime (trialDivision)

import Hilbert.Prime.List.Strict (primesTo')

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "primesTo'" $ do
         it ("correct up to " ++ (show testLimit)) $ do 
           let expected = filter trialDivision [1..]
           let actual = primesTo' testLimit
           sequence_ $ take testLimit $ zipWith shouldBe actual expected

{-
   Supplementary data/functions
-}
testLimit :: Int
testLimit = 10000
