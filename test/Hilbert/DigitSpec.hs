module Hilbert.DigitSpec
  ( main
  , spec
  ) where

import Hilbert.Digit (numDigits, sumDigits, toDigits, fromDigits)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Applicative (liftA)
import Data.Int (Int16)

main :: IO ()
main = hspec spec

{-
   Parameters
-}
-- The maximum size of the number of digits in these tests
maxNumDigits = 100 :: Int

-- The number of tests to conduct of each type
numberOfTests = 1000 :: Int

{-
   Tests
-}
spec = modifyMaxSuccess (\_ -> numberOfTests) $ do
          describe "numDigits" $ do
            smallCases_numDigits
            fixedPrecision_numDigits
            arbitraryPrecision_numDigits
          describe "sumDigits" $ do
            smallCases_sumDigits
            fixedPrecision_sumDigits
            arbitraryPrecision_sumDigits
          describe "toDigits" $ do 
            smallCases_toDigits
            fixedPrecision_toDigits
            arbitraryPrecision_toDigits
          describe "fromDigits" $ do
            emptyListTest_fromDigits
            leadingZero_fromDigits 
            onlyZeros_fromDigits
            fixedPrecision_fromDigits
            arbitraryPrecision_fromDigits

{-
   numDigits tests
-}
smallCases_numDigits = 
  it singleDigitMessage $ do
    forAll digitsGen $ \x -> 
      (numDigits x) `shouldBe` 1

fixedPrecision_numDigits = 
  it fixedPrecisionMessage $ do
    forAll smallGen $ \x -> 
           (numDigits ((fromIntegral x) :: Int16))
        == (numDigits ((fromIntegral x) :: Integer))

arbitraryPrecision_numDigits = 
  it arbitraryPrecisionMessage $ do
    forAll withoutLeadingZero $ \x -> 
      (numDigits (listToInteger x)) `shouldBe` (length x)

{-
   sumDigits tests
-}
smallCases_sumDigits = 
  it singleDigitMessage $ do 
    forAll digitsGen $ \x -> 
      (sumDigits x) `shouldBe` x

fixedPrecision_sumDigits = 
  it fixedPrecisionMessage $ do
    forAll smallGen $ \x -> 
      (toInteger (sumDigits ((fromIntegral x) :: Int16)))
        == (sumDigits ((fromIntegral x) :: Integer))

arbitraryPrecision_sumDigits = 
  it arbitraryPrecisionMessage $ do
    forAll withoutLeadingZero $ \x -> 
      (sumDigits (listToInteger x)) `shouldBe` (fromIntegral $ sum x)

{-
   toDigits tests
-}
smallCases_toDigits = 
  it singleDigitMessage $ do
    forAll digitsGen $ \x -> 
      (toDigits x) `shouldBe` [x]

fixedPrecision_toDigits = 
  it fixedPrecisionMessage $ do
    forAll smallGen $ \x -> 
      (toDigits ((fromIntegral x) :: Int16))
        == (toDigits ((fromIntegral x) :: Integer))

arbitraryPrecision_toDigits = 
  it arbitraryPrecisionMessage $ do
    forAll withoutLeadingZero $ \x -> 
      (toDigits (listToInteger x)) `shouldBe` x

{- 
   fromDigits tests
-}
emptyListTest_fromDigits = 
  it "fromDigits [] == 0" $ do
    (fromDigits []) `shouldBe` 0

leadingZero_fromDigits = 
  it "works on lists starting with zero" $ do
    forAll withLeadingZero $ \xs ->
      (fromDigits xs) `shouldBe` (listToInteger xs)

onlyZeros_fromDigits = 
  it "works on lists of only zeros" $ do
    forAll zeroList $ \xs -> 
      (fromDigits xs) `shouldBe` 0

fixedPrecision_fromDigits =
  it fixedPrecisionMessage $ do
    let bound = maxBound :: Int16
    let actual = (map (fromIntegral . fromDigits . toDigits) [1..bound]) :: [Int16]
    let expected = [1..bound]
    sequence_ $ zipWith shouldBe actual expected

arbitraryPrecision_fromDigits = 
  it "works on arbitrary length lists" $ do
    forAll numbersGen $ \xs -> 
      (fromDigits xs) `shouldBe` (listToInteger xs)
{-
   Extra data
-}
-- The generator for the number of digits in the test case
numDigitsGen :: Gen Int
numDigitsGen = choose (1, maxNumDigits)

-- fixed precision integer generator
smallGen :: Gen Int16
smallGen = choose (1, maxBound :: Int16)

-- The generator for the digits to use in the tests
digitsGen :: Gen Int
digitsGen = choose (0, 9)

-- Numbers with and without leading zeros
withLeadingZero :: Gen [Int]
withLeadingZero = suchThat numbersGen ((== 0) . head) 

withoutLeadingZero :: Gen [Int]
withoutLeadingZero = suchThat numbersGen ((/= 0) . head)

-- Lists of only zeros
zeroList :: Gen [Int]
zeroList = do
    caseLength <- numDigitsGen
    vectorOf caseLength (return 0)

-- Lists of digits of arbitrary length
numbersGen :: Gen [Int]
numbersGen = numDigitsGen >>= (\x ->
             vectorOf x digitsGen)

-- Convert a list of ints to an integer
listToInteger :: [Int] -> Integer
listToInteger = read . concat . (map show)

-- Shared message strings
singleDigitMessage = "works on single digit numbers"
fixedPrecisionMessage = "works on fixed precision integers"
arbitraryPrecisionMessage = "works on arbitrary precision integers"
