module Math.CombinatoricsSpec (spec) where

import Test.Hspec (Spec, it, describe, shouldBe)

import Math.Combinatorics (numPartitions)

spec :: Spec
spec = do
    describe "numParititions" numPartitionsSpec
    
numPartitionsSpec :: Spec
numPartitionsSpec = do
    it "negative numbers have no partitions" $ do
        numPartitions (-1) `shouldBe` 0
        numPartitions (-2) `shouldBe` 0
    it "numPartitions 0 == 1" $ numPartitions 0 `shouldBe` 1
    it "numPartitions 100 correct" $
        numPartitions 100 `shouldBe` 190569292
    it "numPartitions 1000 correct" $
        numPartitions 1000 `shouldBe` 24061467864032622473692149727991
