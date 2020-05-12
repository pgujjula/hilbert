module Math.CombinatoricsSpec (spec) where

import Test.Hspec         (Spec, describe, it, shouldBe)

import Math.Combinatorics (numPartitions)

spec :: Spec
spec = describe "numParititions" numPartitionsSpec

numPartitionsSpec :: Spec
numPartitionsSpec = do
    it "negative numbers have no partitions" $ do
        numPartitions (-1) `shouldBe` 0
        numPartitions (-2) `shouldBe` 0
    it "numPartitions n correct for 0 <= n <= 10" $
        map numPartitions [0..10] `shouldBe`
            [1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42]
    it "numPartitions 100 correct" $
        numPartitions 100 `shouldBe` 190569292
    it "numPartitions 1000 correct" $
        numPartitions 1000 `shouldBe` 24061467864032622473692149727991
