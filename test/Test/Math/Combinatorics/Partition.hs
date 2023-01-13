module Test.Math.Combinatorics.Partition (tests) where

import Math.Combinatorics.Partition (numPartitions, partitions)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Math.Combinatorics.Partition"
    [ numPartitionsTest,
      partitionsTest
    ]

numPartitionsTest :: TestTree
numPartitionsTest =
  testGroup
    "numPartitions tests"
    [ testCase "negative numbers have no partitions" $ do
        numPartitions (-1) @?= 0
        numPartitions (-2) @?= 0,
      testCase "numPartitions n correct for 0 <= n <= 10" $
        map numPartitions [0 .. 10]
          @?= [1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42],
      testCase "numPartitions 100 correct" $
        numPartitions 100 @?= 190569292,
      testCase "numPartitions 1000 correct" $
        numPartitions 1000 @?= 24061467864032622473692149727991
    ]

partitionsTest :: TestTree
partitionsTest =
  testGroup
    "partitions tests"
    [ testCase "no partitions of negative numbers" $ do
        partitions (-1) @?= []
        partitions (-3) @?= [],
      testCase "only partition of 0 is the empty set" $ do
        partitions 0 @?= [[]],
      testCase "works for small inputs" $ do
        partitions 1 @?= [[1]]
        partitions 2 @?= [[2], [1, 1]]
        partitions 3 @?= [[3], [2, 1], [1, 1, 1]]
        partitions 4 @?= [[4], [3, 1], [2, 2], [2, 1, 1], [1, 1, 1, 1]]
        partitions 5
          @?= [ [5],
                [4, 1],
                [3, 2],
                [3, 1, 1],
                [2, 2, 1],
                [2, 1, 1, 1],
                [1, 1, 1, 1, 1]
              ]
    ]
