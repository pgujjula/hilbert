module Math.NumberTheory.DiophantineSpec (spec) where

import Math.NumberTheory.Diophantine
  ( primitivePythagoreanTriples,
    pythagoreanTriples,
    solvePell,
  )
import Math.NumberTheory.Power (integralSqrt, isSquare)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, choose, forAll, suchThat, (===))

spec :: Spec
spec = do
  describe "solvePell" solvePellSpec
  describe "primitivePythagoreanTriples" primitivePythagoreanTriplesSpec
  describe "pythagoreanTriples" pythagoreanTriplesSpec

dmax :: Integer
dmax = 1000

solvePellSpec :: Spec
solvePellSpec = do
  it "d <= 0" $ do
    solvePell (-1) `shouldBe` []
    solvePell 0 `shouldBe` []
  it "d == 1" $
    solvePell 1 `shouldBe` [(1, 0)]
  it "arbitrary squares" $ do
    let gen :: Gen Integer
        gen = (^ 2) <$> choose (2, integralSqrt dmax)
    forAll gen $ \d -> solvePell d === []
  it "arbitrary nonsquares" $ do
    let gen :: Gen Integer
        gen = choose (2, dmax) `suchThat` (not . isSquare)

        correct :: Integer -> Bool
        correct d = all valid $ take 10 $ solvePell d
          where
            valid (x, y) = x ^ 2 - d * y ^ 2 == 1
    forAll gen correct

pythagoreanTriplesSpec :: Spec
pythagoreanTriplesSpec =
  it "first 20 elements correct" $
    take 20 pythagoreanTriples
      `shouldBe` [ (3, 4, 5),
                   (6, 8, 10),
                   (5, 12, 13),
                   (9, 12, 15),
                   (8, 15, 17),
                   (12, 16, 20),
                   (7, 24, 25),
                   (15, 20, 25),
                   (10, 24, 26),
                   (20, 21, 29),
                   (18, 24, 30),
                   (16, 30, 34),
                   (21, 28, 35),
                   (12, 35, 37),
                   (15, 36, 39),
                   (24, 32, 40),
                   (9, 40, 41),
                   (27, 36, 45),
                   (14, 48, 50),
                   (30, 40, 50)
                 ]

primitivePythagoreanTriplesSpec :: Spec
primitivePythagoreanTriplesSpec =
  it "first 20 elements correct" $
    take 20 primitivePythagoreanTriples
      `shouldBe` [ (3, 4, 5),
                   (5, 12, 13),
                   (8, 15, 17),
                   (7, 24, 25),
                   (20, 21, 29),
                   (12, 35, 37),
                   (9, 40, 41),
                   (28, 45, 53),
                   (11, 60, 61),
                   (16, 63, 65),
                   (33, 56, 65),
                   (48, 55, 73),
                   (13, 84, 85),
                   (36, 77, 85),
                   (39, 80, 89),
                   (65, 72, 97),
                   (20, 99, 101),
                   (60, 91, 109),
                   (15, 112, 113),
                   (44, 117, 125)
                 ]
