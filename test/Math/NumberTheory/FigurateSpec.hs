module Math.NumberTheory.FigurateSpec (spec) where

import Math.NumberTheory.Figurate (triangular, triangularN)
import Test.Hspec (Spec, describe, it, shouldBe)

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
    take 1000 (map triangularN [0 ..]) `shouldBe` take 1000 triangular
