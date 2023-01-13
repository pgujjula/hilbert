{-# LANGUAGE ScopedTypeVariables #-}

module Test.Math.NumberTheory.Digit (tests) where

import Control.Monad (forM_)
import Data.Proxy (Proxy (Proxy))
import Math.NumberTheory.Digit (fromDigits, numDigits, sumDigits, toDigits)
import System.Random (Random)
import Test.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    choose,
    elements,
    forAll,
    listOf,
    oneof,
    resize,
    sized,
    suchThat,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Util (throwsException)

maxNumDigits :: Int
maxNumDigits = 50

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Digit"
    [ numDigitsTest,
      sumDigitsTest,
      fromDigitsTest,
      toDigitsTest
    ]

numDigitsTest :: TestTree
numDigitsTest =
  testGroup
    "numDigits tests"
    [ testGroup "single-digit" $
        let testDigits :: (Integral a) => [a] -> Assertion
            testDigits ds = forM_ ds $ \x -> numDigits x @?= 1
         in [ testCase "Ints" $
                testDigits (signedDigits :: [Int]),
              testCase "Integers" $
                testDigits (signedDigits :: [Integer])
            ],
      testGroup "arbitrary length" $
        let naive :: (Integral a, Show a) => a -> Int
            naive = length . show . abs

            testArbitrary :: (Integral a, Show a) => a -> Property
            testArbitrary x = numDigits x === naive x
         in [ testProperty "Ints" $
                forAll uniformLengthIntGen testArbitrary,
              testProperty "Integers" $
                forAll uniformLengthIntegerGen testArbitrary
            ]
    ]

sumDigitsTest :: TestTree
sumDigitsTest =
  testGroup
    "sumDigits"
    [ testGroup "single-digit" $
        let testDigits :: (Integral a) => [a] -> Assertion
            testDigits ds = forM_ ds $ \x ->
              sumDigits x @?= fromIntegral (abs x)
         in [ testCase "Ints" $
                testDigits (signedDigits :: [Int]),
              testCase "Integers" $
                testDigits (signedDigits :: [Integer])
            ],
      testGroup "arbitrary-length" $
        let naive :: (Integral a, Show a) => a -> Int
            naive = sum . map (read . (: [])) . show . abs

            testArbitrary :: (Integral a, Show a) => a -> Property
            testArbitrary x = sumDigits x === naive x
         in [ testProperty "Ints" $
                forAll uniformLengthIntGen testArbitrary,
              testProperty "Integers" $
                forAll uniformLengthIntegerGen testArbitrary
            ]
    ]

fromDigitsTest :: TestTree
fromDigitsTest =
  testGroup
    "fromDigits"
    [ testCase "invalid input" $
        throwsException (fromDigits [1, 2, -1, 0]),
      testGroup
        "empty list"
        [ testCase "empty list, output type Int" $ hunitTest int [] 0,
          testCase "empty list, output type Integer" $ hunitTest integer [] 0
        ],
      testGroup "single-digit" $
        let testDigits :: (Integral a, Show a) => Proxy a -> [a] -> Assertion
            testDigits proxy ds = forM_ ds $ \x ->
              hunitTest proxy [fromIntegral x] x
         in [ testCase "single-digit inputs, output type Int" $
                testDigits int digits,
              testCase "single-digit inputs, output type Integer" $
                testDigits integer digits
            ],
      testGroup "arbitrary length" $
        let naive :: (Read a) => [Int] -> a
            naive = read . concatMap show . (0 :)

            qcTest :: forall a. (Integral a) => Proxy a -> [Int] -> Property
            qcTest _ xs =
              toInteger (fromDigits xs :: a) === (naive xs :: Integer)

            digitListGen :: Gen [Int]
            digitListGen = listOf (elements digits)

            -- Lists that will not cause Int overflow
            smallListGen :: Gen [Int]
            smallListGen = digitListGen `suchThat` (not . overflow)
              where
                overflow xs =
                  (naive xs :: Integer) > toInteger (maxBound :: Int)
         in [ testProperty "arbitrary inputs, output type Int" $
                forAll smallListGen (qcTest int),
              testProperty "arbitrary inputs, output type Integer" $
                forAll digitListGen (qcTest integer)
            ]
    ]
  where
    int = Proxy :: Proxy Int
    integer = Proxy :: Proxy Integer
    hunitTest ::
      (Integral a, Show a) =>
      Proxy a ->
      [Int] ->
      a ->
      Assertion
    hunitTest _ xs n = fromDigits xs @?= n

toDigitsTest :: TestTree
toDigitsTest =
  testGroup
    "toDigits"
    [ testCase "invalid input" $
        throwsException (toDigits (-12)),
      testGroup "single-digit" $
        let testDigits :: (Integral a) => [a] -> Assertion
            testDigits ds = forM_ ds $ \x -> toDigits x @?= [fromIntegral x]
         in [ testCase "single-digit Ints" $
                testDigits (digits :: [Int]),
              testCase "single-digit Integers" $
                testDigits (digits :: [Integer])
            ],
      testCase "three digits" $ toDigits 345 @?= [3, 4, 5],
      testGroup "arbitrary length" $
        let naive :: (Show a) => a -> [Int]
            naive = map (read . (: [])) . show

            test :: (Integral a, Show a) => a -> Property
            test x = toDigits x === naive x
         in [ testProperty "arbitrary length Int" $
                forAll (arbitrary `suchThat` (>= 0) :: Gen Int) test,
              testProperty "arbitrary length Integer" $
                forAll (arbitrary `suchThat` (>= 0) :: Gen Integer) test
            ]
    ]

{- Utilities -}
digits :: (Integral a) => [a]
digits = [0 .. 9]

signedDigits :: (Integral a) => [a]
signedDigits = [-9 .. 9]

-- Generate integers where the length in base 10 is uniformly distributed.
-- Take care to prevent overflow.
uniformLengthIntegralGen :: (Random a, Integral a) => Gen a
uniformLengthIntegralGen = sized $ \n -> oneof $ map genLength [1 .. n]
  where
    genLength i = oneof [positive, negative]
      where
        lower = 10 ^ (i - 1)
        upper = 10 ^ i - 1
        positive = choose (lower, upper)
        negative = choose (-upper, -lower)

uniformLengthIntGen :: Gen Int
uniformLengthIntGen = resize maxIntLength uniformLengthIntegralGen
  where
    maxIntLength = length (show (maxBound :: Int)) - 1

uniformLengthIntegerGen :: Gen Integer
uniformLengthIntegerGen = resize maxNumDigits uniformLengthIntegralGen
