module Test.Math.NumberTheory.Figurate (spec, tests) where

import Math.NumberTheory.Figurate (triangular, triangularN)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = testSpec "Math.NumberTheory.Figurate" spec

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
