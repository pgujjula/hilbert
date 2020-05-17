module Math.NumberTheory.Prime.FactorSpec (spec) where

import Control.Monad                  (forM_, zipWithM_)
import Data.Maybe                     (fromJust)

import Test.Hspec                     (Spec, describe, it, shouldBe)
import Test.QuickCheck                (Gen, choose, forAll, (===))

import Math.NumberTheory.Prime.Factor (factor, factorizations, multiply, pow,
                                       simplify)

spec :: Spec
spec = do
    describe "multiply" multiplySpec
    describe "pow" powSpec
    describe "simplify" simplifySpec
    describe "factor" factorSpec
    describe "factorizations" factorizationsSpec

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
factorSpec =
    it "correct up to limit" $
        forM_ (zip factorizations [1..limit]) $ \(fact, x) ->
            simplify fact `shouldBe` x

factorizationsSpec :: Spec
factorizationsSpec = do
    it "can't factor 0" $
        factor 0 `shouldBe` Nothing
    it "correct up to limit" $
        zipWithM_ shouldBe (map (fromJust . factor) [1..limit]) factorizations
