module Math.NumberTheory.PrimeSpec (spec) where

import Test.Hspec (it, describe, Spec, shouldBe)
import Test.QuickCheck (forAll, choose, (===))

import Math.NumberTheory.Power (integralSqrt)
import Math.NumberTheory.Prime (isPrime, primes, primesTo)

spec :: Spec
spec = do
    describe "isPrime" isPrimeSpec
    describe "primes" primesSpec
    describe "primesTo" primesToSpec
--    describe "factor" factorSpec
--    describe "factorizations" factorizationsSpec
--    describe "factorizationsTo" factorizationsToSpec

naiveIsPrime :: (Integral a) => a -> Bool
naiveIsPrime n = n >= 2 && (not $ any (\d -> n `rem` d == 0) [2..integralSqrt n])

isPrimeSpec :: Spec
isPrimeSpec = do
    it "negative numbers not prime" $ do
        isPrime (-1) `shouldBe` False
        isPrime (-2) `shouldBe` False
    it "0 not prime" $ do
        isPrime 0 `shouldBe` False
    it "1 not prime" $ do
        isPrime 1 `shouldBe` False

    it "primes up to 1000 correct" $ do
        filter isPrime [1..1000] `shouldBe` filter naiveIsPrime [1..1000]

    it "works on arbitrary Ints" $
        forAll (choose (1 :: Int, 100000)) $ \x -> isPrime x === naiveIsPrime x

primesSpec :: Spec
primesSpec =
    it "correct for primes up to 10000" $ do
        takeWhile (<= 10000) primes `shouldBe` (filter naiveIsPrime [1..10000])

primesToSpec :: Spec
primesToSpec =
    it "correct for primes up to 10000" $ do
        primesTo 10000 `shouldBe` (filter naiveIsPrime [1..10000])

--factorSpec :: Spec
--factorSpec = undefined :: Spec
--
--factorizationsSpec :: Spec
--factorizationsSpec = undefined :: Spec
--
--factorizationsToSpec :: Spec
--factorizationsToSpec = undefined :: Spec
