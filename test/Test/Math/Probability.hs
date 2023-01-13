module Test.Math.Probability (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (map)

tests :: TestTree
tests =
  testGroup
    "Math.Probability"
    [ fromToListTest,
      certainTest,
      uniformTest,
      toMapTest,
      probTest,
      expectedValueTest,
      varianceTest,
      mapTest,
      lift2Test,
      bindTest
    ]

fromToListTest :: TestTree
fromToListTest =
  testGroup
    "fromToList tests"
    [ testCase "1 == 1" $
        (1 :: Int) @?= 1
    ]

certainTest :: TestTree
certainTest =
  testGroup
    "certain tests"
    [ testCase "1 == 1" $
        (1 :: Int) @?= 1
    ]

uniformTest :: TestTree
uniformTest =
  testGroup
    "uniform tests"
    [ testCase "1 == 1" $
        (1 :: Int) @?= 1
    ]

toMapTest :: TestTree
toMapTest =
  testGroup
    "toMap tests"
    [ testCase "1 == 1" $
        (1 :: Int) @?= 1
    ]

probTest :: TestTree
probTest =
  testGroup
    "prob tests"
    [ testCase "1 == 1" $
        (1 :: Int) @?= 1
    ]

expectedValueTest :: TestTree
expectedValueTest =
  testGroup
    "expectedValue tests"
    [ testCase "1 == 1" $
        (1 :: Int) @?= 1
    ]

varianceTest :: TestTree
varianceTest =
  testGroup
    "variance tests"
    [ testCase "1 == 1" $
        (1 :: Int) @?= 1
    ]

mapTest :: TestTree
mapTest =
  testGroup
    "map tests"
    [ testCase "1 == 1" $
        (1 :: Int) @?= 1
    ]

lift2Test :: TestTree
lift2Test =
  testGroup
    "lift2 tests"
    [ testCase "1 == 1" $
        (1 :: Int) @?= 1
    ]

bindTest :: TestTree
bindTest =
  testGroup
    "bind tests"
    [ testCase "1 == 1" $
        (1 :: Int) @?= 1
    ]
