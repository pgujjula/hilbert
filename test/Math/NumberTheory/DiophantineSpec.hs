module Math.NumberTheory.DiophantineSpec (spec) where

import Test.Hspec (describe, shouldBe, it, Spec)
import Test.QuickCheck ((===), forAll, arbitrary, suchThat, Gen, Property, choose)

import Math.NumberTheory.Diophantine (solvePell)
import Math.NumberTheory.Power (isSquare, integralSqrt)

spec :: Spec
spec = do
    describe "solvePell" solvePellSpec

dmax :: Integer
dmax = 1000

solvePellSpec :: Spec
solvePellSpec = do
    it "d <= 0" $ do
        solvePell (-1) `shouldBe` []
        solvePell 0 `shouldBe` []
    it "d == 1" $ do
        solvePell 1 `shouldBe` [(1, 0)]
    it "arbitrary squares" $ do
        let gen :: Gen Integer
            gen = (^2) <$> choose (2, integralSqrt dmax)
        forAll gen $ \d -> solvePell d === []
    it "arbitrary nonsquares" $ do
        let gen :: Gen Integer
            gen = choose (2, dmax) `suchThat` (not . isSquare)
            
            correct :: Integer -> Bool
            correct d = all valid $ take 10 $ solvePell d
                where valid (x, y) = x^2 - d*y^2 == 1
        forAll gen correct
