module Math.NumberTheory.DivisorSpec (spec) where

import Control.Monad                  (forM_)
import Data.List                      (sort)
import Data.Maybe                     (fromJust)

import Test.Hspec                     (Spec, describe, it, shouldBe)

import Math.NumberTheory.Divisor      (divides, divisorPairs, divisorPairsF,
                                       divisors, divisorsF, numDivisors,
                                       numDivisorsF)
import Math.NumberTheory.Prime.Factor (factor)

limit :: Int
limit = 1000

spec :: Spec
spec = do
    describe "divides"       dividesSpec
    describe "divisors"      divisorsSpec
    describe "divisorsF"     divisorsFSpec
    describe "divisorPairs"  divisorPairsSpec
    describe "divisorPairsF" divisorPairsFSpec
    describe "numDivisors"   numDivisorsSpec
    describe "numDivisorsF"  numDivisorsFSpec

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
