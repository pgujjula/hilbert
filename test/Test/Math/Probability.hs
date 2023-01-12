module Test.Math.Probability (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (map)

spec :: Spec
spec = do
  describe "fromToList" fromToListSpec
  describe "certain" certainSpec
  describe "uniform" uniformSpec
  describe "toMap" toMapSpec

  describe "prob" probSpec
  describe "expectedValue" expectedValueSpec
  describe "variance" varianceSpec

  describe "map" mapSpec
  describe "lift2" lift2Spec
  describe "bind" bindSpec

fromToListSpec :: Spec
fromToListSpec = it "1 == 1" $ 1 `shouldBe` 1

certainSpec :: Spec
certainSpec = it "1 == 1" $ 1 `shouldBe` 1

uniformSpec :: Spec
uniformSpec = it "1 == 1" $ 1 `shouldBe` 1

toMapSpec :: Spec
toMapSpec = it "1 == 1" $ 1 `shouldBe` 1

probSpec :: Spec
probSpec = it "1 == 1" $ 1 `shouldBe` 1

expectedValueSpec :: Spec
expectedValueSpec = it "1 == 1" $ 1 `shouldBe` 1

varianceSpec :: Spec
varianceSpec = it "1 == 1" $ 1 `shouldBe` 1

mapSpec :: Spec
mapSpec = it "1 == 1" $ 1 `shouldBe` 1

lift2Spec :: Spec
lift2Spec = it "1 == 1" $ 1 `shouldBe` 1

bindSpec :: Spec
bindSpec = it "1 == 1" $ 1 `shouldBe` 1
