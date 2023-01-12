module Test.Math.Combinatorics.Partition (spec) where

import Math.Combinatorics.Partition (numPartitions, partitions)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "numParititions" numPartitionsSpec
  describe "partitions" partitionsSpec

numPartitionsSpec :: Spec
numPartitionsSpec = do
  it "negative numbers have no partitions" $ do
    numPartitions (-1) `shouldBe` 0
    numPartitions (-2) `shouldBe` 0
  it "numPartitions n correct for 0 <= n <= 10" $
    map numPartitions [0 .. 10]
      `shouldBe` [1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42]
  it "numPartitions 100 correct" $
    numPartitions 100 `shouldBe` 190569292
  it "numPartitions 1000 correct" $
    numPartitions 1000 `shouldBe` 24061467864032622473692149727991

partitionsSpec :: Spec
partitionsSpec = do
  it "no partitions of negative numbers" $ do
    partitions (-1) `shouldBe` []
    partitions (-3) `shouldBe` []
  it "only partition of 0 is the empty set" $ do
    partitions 0 `shouldBe` [[]]
  it "works for small inputs" $ do
    partitions 1 `shouldBe` [[1]]
    partitions 2 `shouldBe` [[2], [1, 1]]
    partitions 3 `shouldBe` [[3], [2, 1], [1, 1, 1]]
    partitions 4 `shouldBe` [[4], [3, 1], [2, 2], [2, 1, 1], [1, 1, 1, 1]]
    partitions 5
      `shouldBe` [[5], [4, 1], [3, 2], [3, 1, 1], [2, 2, 1], [2, 1, 1, 1], [1, 1, 1, 1, 1]]
