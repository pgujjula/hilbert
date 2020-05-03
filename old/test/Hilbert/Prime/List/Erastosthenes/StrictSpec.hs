module Hilbert.Prime.List.Erastosthenes.StrictSpec
  ( main
  , spec
  ) where

import Test.Hspec (hspec, Spec, describe, it, shouldBe)
import Hilbert.Prime (isPrime)

import Hilbert.Prime.List.Erastosthenes (primesTo')

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "primesTo'" $ do
         it ("correct up to " ++ (show testLimit)) $ do 
           let expected = filter isPrime [1..]
           let actual = primesTo' testLimit
           sequence_ $ take testLimit $ zipWith shouldBe actual expected

{-
   Supplementary data/functions
-}
testLimit :: Int
testLimit = 10000
