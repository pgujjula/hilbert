module Hilbert.Prime.List.LazySpec
  ( main
  , spec
  ) where

import Test.Hspec (hspec, Spec, describe, it, shouldBe)
import Hilbert.Prime (trialDivision)

import Hilbert.Prime.List.Lazy (primes, primesTo)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "primes" $ do
    it ("correct up to " ++ (show testLimit)) $ do
      let expected = filter trialDivision [1..]
      let actual = primes
      sequence_ $ take testLimit $ zipWith shouldBe actual expected

  describe "primesTo" $ do
    it ("correct up to " ++ (show testLimit)) $ do
      let expected = filter trialDivision [1..]
      let actual = primesTo testLimit
      sequence_ $ zipWith shouldBe actual expected

{-
   Supplementary data/functions
-}
testLimit = 10000
