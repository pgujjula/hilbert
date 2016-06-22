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

spec = modifyMaxSuccess (\_ -> numberOfTests) $
        describe "Digit" $ do
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
            arbitraryPrecision_fromDigits
            fixedPrecision_fromDigits

{-
   Parameters
-}
-- The maximum size of the number of digits in these tests
maxNumDigits = 100 :: Int

-- The number of tests to conduct of each type
numberOfTests = 1000 :: Int

-- The generator for the number of digits in the test case
numDigitsGen :: Gen Int
numDigitsGen = choose (1, maxNumDigits)

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

-- Only zeros
zeroList :: Gen [Int]
zeroList = do
    caseLength <- numDigitsGen
    vectorOf caseLength (return 0)

-- The generator to create the actual test cases
numbersGen :: Gen [Int]
numbersGen = numDigitsGen >>= (\x ->
             vectorOf x digitsGen)

-- Convert a list of ints to an integer
listToInteger :: [Int] -> Integer
listToInteger = read . concat . (map show)

{-
   numDigits tests
-}
smallCases_numDigits = 
  it "Works on single digit numbers" $ do
    forAll digitsGen $ \x -> 
      (numDigits x) `shouldBe` 1

fixedPrecision_numDigits = 
  it "Works on fixed precision integers" $ do
    forAll smallGen $ \x -> 
           (numDigits ((fromIntegral x) :: Int16))
        == (numDigits ((fromIntegral x) :: Integer))

arbitraryPrecision_numDigits = 
  it "Works on arbitrarily large positive integers" $ do
    forAll withoutLeadingZero $ \x -> 
      (numDigits (listToInteger x)) `shouldBe` (length x)

{-
   sumDigits tests
-}
smallCases_sumDigits = 
  it "sumDigits works on numbers close to 0" $ do 
    forAll digitsGen $ \x -> 
      (sumDigits x) `shouldBe` x

arbitraryPrecision_sumDigits = 
  it "sumDigits works on arbitrarily large positive integers" $ do
    forAll withoutLeadingZero $ \x -> 
      (sumDigits (listToInteger x)) `shouldBe` (fromIntegral $ sum x)

fixedPrecision_sumDigits = 
  it "has no overflow issues" $ do
    forAll smallGen $ \x -> 
      (toInteger (sumDigits ((fromIntegral x) :: Int16)))
        == (sumDigits ((fromIntegral x) :: Integer))

{-
   toDigits tests
-}
smallCases_toDigits = 
  it "toDigits works on numbers close to 0" $ do
    forAll digitsGen $ \x -> 
      (toDigits x) `shouldBe` [x]

arbitraryPrecision_toDigits = 
  it "toDigits works on arbitrarily large positive integers" $ do
    forAll withoutLeadingZero $ \x -> 
      (toDigits (listToInteger x)) `shouldBe` x

fixedPrecision_toDigits = 
  it "has no overflow issues" $ do
    forAll smallGen $ \x -> 
      (toDigits ((fromIntegral x) :: Int16))
        == (toDigits ((fromIntegral x) :: Integer))

{- 
   fromDigits tests
-}
emptyListTest_fromDigits = 
  it "fromDigits [] == 0" $ do
    (fromDigits []) `shouldBe` 0

leadingZero_fromDigits = 
  it "fromDigits works on lists starting with zero" $ do
    forAll withLeadingZero $ \xs ->
      (fromDigits xs) `shouldBe` (listToInteger xs)

onlyZeros_fromDigits = 
  it "fromDigits works on lists of only zeros" $ do
    forAll zeroList $ \xs -> 
      (fromDigits xs) `shouldBe` 0

fixedPrecision_fromDigits =
  it "Works with fixed precision integers" $ do
    let bound = maxBound :: Int16
    let actual = (map (fromIntegral . fromDigits . toDigits) [1..bound]) :: [Int16]
    let expected = [1..bound]
    sequence_ $ zipWith shouldBe actual expected

arbitraryPrecision_fromDigits = 
  it "fromDigits works on arbitrary lists" $ do
    forAll numbersGen $ \xs -> 
      (fromDigits xs) `shouldBe` (listToInteger xs)
