module Test.Math.NumberTheory.QuadraticField (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Prelude hiding (sqrt)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.QuadraticField"
    [ numTest,
      fractionalTest,
      showTest
    ]

numTest :: TestTree
numTest =
  testGroup
    "instance Num tests"
    [ addTest,
      subTest,
      mulTest,
      absTest,
      negateTest,
      signumTest,
      fromIntegerTest
    ]

addTest :: TestTree
addTest = testCase "addTest" (pure ())

subTest :: TestTree
subTest = testCase "subTest" (pure ())

mulTest :: TestTree
mulTest = testCase "mulTest" (pure ())

absTest :: TestTree
absTest = testCase "absTest" (pure ())

negateTest :: TestTree
negateTest = testCase "negateTest" (pure ())

signumTest :: TestTree
signumTest = testCase "signumTest" (pure ())

fromIntegerTest :: TestTree
fromIntegerTest = testCase "fromIntegerTest" (pure ())

fractionalTest :: TestTree
fractionalTest =
  testGroup
    "instance Fractional tests"
    [fromRationalTest
    , recipTest]

fromRationalTest :: TestTree
fromRationalTest = testCase "fromRationalTest" (pure ())

recipTest :: TestTree
recipTest = testCase "recipTest" (pure ())

showTest :: TestTree
showTest =
  testGroup
    "instance Show tests"
    []
