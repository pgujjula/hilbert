module Math.NumberTheory.DivisorSpec (spec) where

import Control.Exception              (evaluate)
import Control.Monad                  (forM_, zipWithM_)
import Data.List                      (sort)
import Data.Maybe                     (fromJust)

import Test.Hspec                     (Spec, anyException, describe, it,
                                       shouldBe, shouldThrow)

import Math.NumberTheory.Divisor      (divides, divisorPairs, divisorPairsF,
                                       divisors, divisorsF, mobius, mobiusF,
                                       numDivisors, numDivisorsF,
                                       relativelyPrime, sumDivisors,
                                       sumDivisorsF, totient, totientF)
import Math.NumberTheory.Prime.Factor (factor, factorizations)

limit :: Int
limit = 1000

spec :: Spec
spec = do
    describe "divides"         dividesSpec
    describe "divisors"        divisorsSpec
    describe "divisorsF"       divisorsFSpec
    describe "divisorPairs"    divisorPairsSpec
    describe "divisorPairsF"   divisorPairsFSpec

    describe "numDivisors"     numDivisorsSpec
    describe "numDivisorsF"    numDivisorsFSpec

    describe "sumDivisors"     sumDivisorsSpec
    describe "sumDivisorsF"    sumDivisorsFSpec

    describe "relativelyPrime" relativelyPrimeSpec

    describe "totient"         totientSpec
    describe "totientF"        totientFSpec

    describe "mobius"          mobiusSpec
    describe "mobiusF"         mobiusFSpec

dividesSpec :: Spec
dividesSpec = do
    it "0 divides 0" $
        (0 `divides` 0)    `shouldBe` True
    it "0 doesn't divide anything else" $ do
        (0 `divides` 1)    `shouldBe` False
        (0 `divides` (-5)) `shouldBe` False
    it "small cases" $ do
        (5 `divides` 30)   `shouldBe` True
        (5 `divides` 31)   `shouldBe` False

divisorsSpec :: Spec
divisorsSpec =
    it ("correct for abs n up to " ++ show limit) $
        let naive n = filter (`divides` n) [1..abs n]
         in forM_ [(-limit)..limit] $ \x ->
                sort (divisors x) `shouldBe` naive x

divisorsFSpec :: Spec
divisorsFSpec =
    it ("correct for n up to " ++ show limit) $
        let naive n = filter (`divides` n) [1..abs n]
         in forM_ [1..limit] $ \x ->
                (sort . divisorsF . fromJust . factor $ x) `shouldBe` naive x

divisorPairsSpec :: Spec
divisorPairsSpec =
    it ("correct for abs n up to " ++ show limit) $
        forM_ [(-limit)..limit] $ \x -> do
            let dps = divisorPairs x
            length dps
                `shouldBe` ((length (divisors x) + 1) `div` 2)
            mapM_ (\(a, b) -> a * b `shouldBe` abs x) dps

divisorPairsFSpec :: Spec
divisorPairsFSpec =
    it ("correct for n up to " ++ show limit) $
        forM_ [1..limit] $ \x -> do
            let dps = divisorPairsF . fromJust . factor $ x
            length dps
                `shouldBe` ((length (divisors x) + 1) `div` 2)
            mapM_ (\(a, b) -> a * b `shouldBe` x) dps

numDivisorsSpec :: Spec
numDivisorsSpec =
    it ("correct for abs n up to " ++ show limit) $
        let naive n = length $ filter (`divides` n) [1..abs n]
         in forM_ [(-limit)..limit] $ \x -> numDivisors x `shouldBe` naive x

numDivisorsFSpec :: Spec
numDivisorsFSpec =
    it ("correct for n up to " ++ show limit) $
        let naive n = length $ filter (`divides` n) [1..abs n]
         in forM_ [1..limit] $ \x ->
                (numDivisorsF . fromJust . factor $ x) `shouldBe` naive x

sumDivisorsSpec :: Spec
sumDivisorsSpec =
    it ("correct for abs n up to " ++ show limit) $
        let naive n = sum $ filter (`divides` n) [1..abs n]
         in forM_ [(-limit)..limit] $ \x -> sumDivisors x `shouldBe` naive x

sumDivisorsFSpec :: Spec
sumDivisorsFSpec =
    it ("correct for n up to " ++ show limit) $
        let naive n = sum $ filter (`divides` n) [1..abs n]
         in forM_ [1..limit] $ \x ->
                (sumDivisorsF . fromJust . factor $ x) `shouldBe` naive x

relativelyPrimeSpec :: Spec
relativelyPrimeSpec = do
    it "0 is not relatively prime to 0" $
        relativelyPrime 0 0    `shouldBe` False
    it "0 is relatively prime to 1/-1" $ do
        relativelyPrime 0 1    `shouldBe` True
        relativelyPrime 0 (-1) `shouldBe` True
    it "0 is not relatively to integers with magnitude > 1" $ do
        relativelyPrime 0 2    `shouldBe` False
        relativelyPrime 0 (-2) `shouldBe` False
    it "1 is relatively prime to 1/-1" $ do
        relativelyPrime 1 1    `shouldBe` True
        relativelyPrime 1 (-1) `shouldBe` True
    it "3 is relatively prime to 5/-5" $ do
        relativelyPrime 3 5    `shouldBe` True
        relativelyPrime 3 (-5) `shouldBe` True
    it "-3 is not relatively prime to 6/-6" $ do
        relativelyPrime (-3) 6    `shouldBe` False
        relativelyPrime (-3) (-6) `shouldBe` False

totientSpec :: Spec
totientSpec = do
    it "totient of negative numbers is 0" $
        totient (-1) `shouldBe` 0
    it "totient of 0 is 1" $
        totient 0 `shouldBe` 1
    it ("correct for up to " ++ show limit) $
        forM_ [1..limit] $ \n ->
            totient n `shouldBe` length (filter (relativelyPrime n) [1..n])

totientFSpec :: Spec
totientFSpec =
    it ("correct up to " ++ show limit) $
        forM_ [1..limit] $ \n ->
            totientF n (fromJust $ factor n)
                `shouldBe` length (filter (relativelyPrime n) [1..n])

mobiusSpec :: Spec
mobiusSpec = do
  it "mobius of non-positive numbers is undefined" $ do
    evaluate (mobius (-1)) `shouldThrow` anyException
    evaluate (mobius 0) `shouldThrow` anyException
  it "correct up to 10" $ do
    zipWithM_ shouldBe (fmap mobius [1..10])
                       [1,-1,-1,0,-1,1,-1,0,0,1]

mobiusFSpec :: Spec
mobiusFSpec = do
  it "correct up to 10" $ do
    zipWithM_ shouldBe (fmap mobiusF $ take 10 factorizations)
                       [1,-1,-1,0,-1,1,-1,0,0,1]
