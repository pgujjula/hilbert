{-# LANGUAGE ScopedTypeVariables #-}

module Test.Math.NumberTheory.Digit (spec) where

import Control.Monad (forM_)
import Data.Proxy (Proxy (Proxy))
import Math.NumberTheory.Digit (fromDigits, numDigits, sumDigits, toDigits)
import System.Random (Random)
import Test.Hspec
  ( Expectation,
    Spec,
    anyErrorCall,
    describe,
    it,
    shouldBe,
    shouldThrow,
  )
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

maxNumDigits :: Int
maxNumDigits = 50

spec :: Spec
spec = do
  describe "numDigits" numDigitsSpec
  describe "sumDigits" sumDigitsSpec
  describe "fromDigits" fromDigitsSpec
  describe "toDigits" toDigitsSpec

numDigitsSpec :: Spec
numDigitsSpec = do
  let testDigits :: (Integral a) => [a] -> Expectation
      testDigits ds = forM_ ds $ \x -> numDigits x `shouldBe` 1
   in do
        it "single-digit Ints" $
          testDigits (signedDigits :: [Int])
        it "single-digit Integers" $
          testDigits (signedDigits :: [Integer])

  let naive :: (Integral a, Show a) => a -> Int
      naive = length . show . abs

      testArbitrary :: (Integral a, Show a) => a -> Property
      testArbitrary x = numDigits x === naive x
   in do
        it "arbitrary-length Ints" $
          forAll uniformLengthIntGen testArbitrary
        it "arbitrary-length Integers" $
          forAll uniformLengthIntegerGen testArbitrary

sumDigitsSpec :: Spec
sumDigitsSpec = do
  let testDigits :: (Integral a) => [a] -> Expectation
      testDigits ds = forM_ ds $ \x -> sumDigits x `shouldBe` fromIntegral (abs x)
   in do
        it "single-digit Ints" $
          testDigits (signedDigits :: [Int])
        it "single-digit Integers" $
          testDigits (signedDigits :: [Integer])

  let naive :: (Integral a, Show a) => a -> Int
      naive = sum . map (read . (: [])) . show . abs

      testArbitrary :: (Integral a, Show a) => a -> Property
      testArbitrary x = sumDigits x === naive x
   in do
        it "arbitrary-length Ints" $
          forAll uniformLengthIntGen testArbitrary
        it "arbitrary-length Integers" $
          forAll uniformLengthIntegerGen testArbitrary

fromDigitsSpec :: Spec
fromDigitsSpec = do
  it "invalid input" $
    (fromDigits [1, 2, -1, 0] `shouldBe` 0)
      `shouldThrow` anyErrorCall

  let hunitTest ::
        (Integral a, Show a) =>
        Proxy a ->
        [Int] ->
        a ->
        Expectation
      hunitTest _ xs n = fromDigits xs `shouldBe` n

      int = Proxy :: Proxy Int
      integer = Proxy :: Proxy Integer
  it "empty list, output type Int" $ hunitTest int [] 0
  it "empty list, output type Integer" $ hunitTest integer [] 0

  let testDigits :: (Integral a, Show a) => Proxy a -> [a] -> Expectation
      testDigits proxy ds = forM_ ds $ \x ->
        hunitTest proxy [fromIntegral x] x
  it "single-digit inputs, output type Int" $ testDigits int digits
  it "single-digit inputs, output type Integer" $ testDigits integer digits

  let naive :: (Read a) => [Int] -> a
      naive = read . concatMap show . (0 :)

      qcTest :: forall a. (Integral a) => Proxy a -> [Int] -> Property
      qcTest _ xs = toInteger (fromDigits xs :: a) === (naive xs :: Integer)

      digitListGen :: Gen [Int]
      digitListGen = listOf (elements digits)

      -- Lists that will not cause Int overflow
      smallListGen :: Gen [Int]
      smallListGen = digitListGen `suchThat` (not . overflow)
        where
          overflow xs = (naive xs :: Integer) > toInteger (maxBound :: Int)
  it "arbitrary inputs, output type Int" $
    forAll smallListGen (qcTest int)
  it "arbitrary inputs, output type Integer" $
    forAll digitListGen (qcTest integer)

toDigitsSpec :: Spec
toDigitsSpec = do
  it "invalid input" $
    (toDigits (-12) `shouldBe` [1, 2])
      `shouldThrow` anyErrorCall

  let testDigits :: (Integral a) => [a] -> Expectation
      testDigits ds = forM_ ds $ \x -> toDigits x `shouldBe` [fromIntegral x]
  it "single-digit Ints" $ testDigits (digits :: [Int])
  it "single-digit Integers" $ testDigits (digits :: [Integer])
  it "three digits" $ toDigits 345 `shouldBe` [3, 4, 5]

  let naive :: (Show a) => a -> [Int]
      naive = map (read . (: [])) . show

      test :: (Integral a, Show a) => a -> Property
      test x = toDigits x === naive x
  it "arbitrary length Int" $
    forAll (arbitrary `suchThat` (>= 0) :: Gen Int) test
  it "arbitrary length Integer" $
    forAll (arbitrary `suchThat` (>= 0) :: Gen Integer) test

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
