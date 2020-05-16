module Math.NumberTheory.FigurateSpec (spec) where

import Test.Hspec                 (Spec, describe, it, shouldBe)

import Math.NumberTheory.Figurate (triangular, triangularN)

spec :: Spec
spec = do
    describe "triangular" triangularSpec
    describe "triangularN" triangularNSpec

triangularSpec :: Spec
triangularSpec =
    it "first 10 correct" $
        take 10 triangular `shouldBe` [0, 1, 3, 6, 10, 15, 21, 28, 36, 45]

triangularNSpec :: Spec
triangularNSpec =
    it "first 1000 correct" $
        take 1000 (map triangularN [0..]) `shouldBe` take 1000 triangular
