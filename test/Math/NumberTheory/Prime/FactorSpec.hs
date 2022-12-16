{-# LANGUAGE MultiWayIf #-}
module Math.NumberTheory.Prime.FactorSpec (spec) where

import Control.Arrow                  ((>>>))
import Control.Monad                  (forM_, zipWithM_)
import Data.List.Duplicate            (groupAdj)
import Data.Maybe                     (fromJust)
import Math.NumberTheory.Prime.Sieve  (primes)
import Test.Hspec                     (Spec, describe, it, shouldBe)
import Test.QuickCheck                (Gen, choose, forAll, (===))

import Math.NumberTheory.Prime.Factor (Factorization, factor, factorizations,
                                       factorizationsFrom, multiply, pow,
                                       simplify)

spec :: Spec
spec = do
    describe "multiply" multiplySpec
    describe "pow" powSpec
    describe "simplify" simplifySpec
    describe "factor" factorSpec
    describe "factorizations" factorizationsSpec
    describe "factorizationsFrom" factorizationsFromSpec

limit :: (Integral a) => a
limit = 5000

powLimit :: Int
powLimit = 10

multiplySpec :: Spec
multiplySpec =
    it "arbitrary inputs" $
        let gen :: Gen (Int, Int)
            gen = (,) <$> choose (1, limit) <*> choose (1, limit)
         in forAll gen $ \(x, y) ->
                (multiply <$> factor x <*> factor y) === factor (x*y)

powSpec :: Spec
powSpec =
    it "arbitrary inputs" $
        let gen :: Gen (Integer, Int)
            gen = (,) <$> choose (1, limit) <*> choose (1, powLimit)
         in forAll gen $ \(x, k) ->
                pow (fromJust $ factor x) k === fromJust (factor (x^k))

simplifySpec :: Spec
simplifySpec = do
    it "empty list" $
        simplify [] == 1
    it "small input" $ do
        simplify [(2, 3), (3, 1)] `shouldBe` 24
        simplify [(3, 1), (7, 1), (11, 1)] `shouldBe` 231

factorSpec :: Spec
factorSpec = do
    it "can't factor 0" $
        factor 0 `shouldBe` Nothing
    it "correct up to limit" $
        zipWithM_ shouldBe (map (fromJust . factor) [1..limit]) factorizations

factorizationsSpec :: Spec
factorizationsSpec =
    it "correct up to limit" $
      zipWithM_ shouldBe (take limit factorizations) (map factorNaive [1..limit])

factorizationsFromSpec :: Spec
factorizationsFromSpec =
    it "correct up to limit, for many start points" $
      forM_ [1..30] $ \i ->
        take limit (factorizationsFrom i)
        `shouldBe` take limit (drop (i-1) factorizations)

factorNaive :: Int -> Factorization Int
factorNaive =
  findFactors
  >>> groupAdj
  >>> map (\x -> (head x, length x))

findFactors :: Int -> [Int]
findFactors n = findFactorsWith n (map fromIntegral primes)

findFactorsWith :: Int -> [Int] -> [Int]
findFactorsWith _ [] = error "impossible"
findFactorsWith n (p:ps) =
  let (q, r) = n `quotRem` p
   in if | p > n     -> []
         | r == 0    -> p : findFactorsWith q (p:ps)
         | otherwise -> findFactorsWith n ps
