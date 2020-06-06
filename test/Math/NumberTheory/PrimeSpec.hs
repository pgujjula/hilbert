module Math.NumberTheory.PrimeSpec (spec) where

import Test.Hspec              (Spec, describe, it, shouldBe)
import Test.QuickCheck         (choose, forAll, (===))

import Math.NumberTheory.Power (integralSqrt)
import Math.NumberTheory.Prime (composites, compositesTo, isPrime, primes,
                                primesTo, unsafeMarkPrime, unPrime)

spec :: Spec
spec = do
    describe "isPrime" isPrimeSpec
    describe "primes" primesSpec
    describe "primesTo" primesToSpec
    describe "composites" compositesSpec
    describe "compositesTo" compositesToSpec

naiveIsPrime :: (Integral a) => a -> Bool
naiveIsPrime n = n >= 2 && not (any (\d -> n `rem` d == 0) [2..integralSqrt n])

isPrimeSpec :: Spec
isPrimeSpec = do
    it "negative numbers not prime" $ do
        isPrime (-1) `shouldBe` False
        isPrime (-2) `shouldBe` False
    it "0 not prime" $
        isPrime 0 `shouldBe` False
    it "1 not prime" $
        isPrime 1 `shouldBe` False

    it "primes up to 1000 correct" $
        filter isPrime [1..1000] `shouldBe` filter naiveIsPrime [1..1000]

    it "works on arbitrary Ints" $
        forAll (choose (1 :: Int, 100000)) $ \x -> isPrime x === naiveIsPrime x

primesSpec :: Spec
primesSpec =
    it "correct for primes up to 10000" $
        takeWhile (\x -> unPrime x <= 10000) primes
            `shouldBe` map unsafeMarkPrime (filter naiveIsPrime [1..10000])

primesToSpec :: Spec
primesToSpec = do
    it "correct for primes up to 10000" $
        primesTo 10000
            `shouldBe` map unsafeMarkPrime (filter naiveIsPrime [1..10000])
    it "is inclusive" $
        primesTo 97
            `shouldBe` map unsafeMarkPrime (filter naiveIsPrime [1..97])

compositesSpec :: Spec
compositesSpec =
    it "correct for composities up to 10000" $
        takeWhile (<= 10000) composites `shouldBe` filter (not . naiveIsPrime) [2..10000]

compositesToSpec :: Spec
compositesToSpec =
    it "correct for composities up to 10000" $
        compositesTo 10000 `shouldBe` filter (not . naiveIsPrime) [2..10000]
