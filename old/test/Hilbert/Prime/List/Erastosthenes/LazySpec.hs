module Hilbert.Prime.List.Erastosthenes.LazySpec
  ( main
  , spec
  ) where

import Test.Hspec (hspec, Spec, describe, it, shouldBe)
import Hilbert.Prime (isPrime)

import Hilbert.Prime.List.Erastosthenes (primes, primesTo)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "primes" $ do
    it ("correct up to " ++ (show testLimit)) $ do
      let expected = filter isPrime [1..]
      let actual = primes
      sequence_ $ take testLimit $ zipWith shouldBe actual expected

  describe "primesTo" $ do
    it ("correct up to " ++ (show testLimit)) $ do
      let expected = filter isPrime [1..]
      let actual = primesTo testLimit
      sequence_ $ zipWith shouldBe actual expected

{-
   Supplementary data/functions
-}
testLimit = 10000
