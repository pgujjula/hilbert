module Test.Math.NumberTheory.Figurate (tests) where

import Math.NumberTheory.Figurate (triangular, triangularN)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Figurate"
    [ triangularTest,
      triangularNTest
    ]

triangularTest :: TestTree
triangularTest =
  testGroup
    "triangular test"
    [ testCase "first 10 correct" $
        take 10 triangular @?= [0, 1, 3, 6, 10, 15, 21, 28, 36, 45]
    ]

triangularNTest :: TestTree
triangularNTest =
  testGroup
    "triangularN test"
    [ testCase "first 1000 correct" $
        take 1000 (map triangularN [0 ..]) @?= take 1000 triangular
    ]
