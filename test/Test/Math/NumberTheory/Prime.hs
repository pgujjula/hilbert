module Test.Math.NumberTheory.Prime (spec) where

import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.Prime
  ( composites,
    compositesTo,
    isPrime,
    primes,
    primesFromTo,
    primesTo,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (choose, forAll, (===))

spec :: Spec
spec = do
  describe "isPrime" isPrimeSpec
  describe "primes" primesSpec
  describe "primesTo" primesToSpec
  describe "primesFromTo" primesFromToSpec
  describe "composites" compositesSpec
  describe "compositesTo" compositesToSpec

naiveIsPrime :: (Integral a) => a -> Bool
naiveIsPrime n = n >= 2 && not (any (\d -> n `rem` d == 0) [2 .. integerSquareRoot n])

isPrimeSpec :: Spec
isPrimeSpec = do
  it "negative numbers not prime" $ do
    isPrime (-1) `shouldBe` False
    isPrime (-2) `shouldBe` False
    -- (-1995) is prime when cast to a Word, so this checks that we are
    -- checking for negative numbers before casting.
    isPrime (-1995) `shouldBe` False
  it "0 not prime" $
    isPrime 0 `shouldBe` False
  it "1 not prime" $
    isPrime 1 `shouldBe` False

  it "primes up to 1000 correct" $
    filter isPrime [1 .. 1000] `shouldBe` filter naiveIsPrime [1 .. 1000]

  it "works on arbitrary Ints" $
    forAll (choose (1 :: Int, 100000)) $
      \x -> isPrime x === naiveIsPrime x

primesSpec :: Spec
primesSpec =
  it "correct for primes up to 10000" $
    takeWhile (<= 10000) primes
      `shouldBe` filter naiveIsPrime [1 .. 10000]

primesToSpec :: Spec
primesToSpec = do
  it "correct for degenerate cases" $ do
    primesTo (-1) `shouldBe` []
    primesTo 0 `shouldBe` []
    primesTo 1 `shouldBe` []
    primesTo 2 `shouldBe` [2]
    primesTo 3 `shouldBe` [2, 3]
  it "correct for primes up to 10000" $
    primesTo 10000
      `shouldBe` filter naiveIsPrime [1 .. 10000]
  it "is inclusive" $
    primesTo 97
      `shouldBe` filter naiveIsPrime [1 .. 97]

primesFromToSpec :: Spec
primesFromToSpec = do
  it "correct for degenerate cases" $ do
    primesFromTo 5 3 `shouldBe` []
    primesFromTo 0 1 `shouldBe` []
    primesFromTo 0 2 `shouldBe` [2]
    primesFromTo 0 3 `shouldBe` [2, 3]
  it "correct for random starts and stops" $ do
    let limit = 10000
    forAll (choose (1, limit)) $ \lower -> do
      forAll (choose (lower, limit)) $ \upper ->
        do
          primesFromTo lower upper
          `shouldBe` filter isPrime [lower .. upper]

compositesSpec :: Spec
compositesSpec =
  it "correct for composities up to 10000" $
    takeWhile (<= 10000) composites `shouldBe` filter (not . naiveIsPrime) [2 .. 10000]

compositesToSpec :: Spec
compositesToSpec =
  it "correct for composities up to 10000" $
    compositesTo 10000 `shouldBe` filter (not . naiveIsPrime) [2 .. 10000]
