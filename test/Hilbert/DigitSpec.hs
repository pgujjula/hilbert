module Hilbert.DigitSpec (main, spec) where

import Hilbert.Digit (numDigits, sumDigits, toDigits, fromDigits)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Applicative (liftA)

main :: IO ()
main = hspec spec

-- The maximum size of the number of digits in these tests
maxNumDigits = 100 :: Int

-- The number of tests to conduct of each type
numberOfTests = 1000 :: Int

-- The generator for the number of digits in the test case
numDigitsGen :: Gen Int
numDigitsGen = choose (1, maxNumDigits)

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

spec = modifyMaxSuccess (\_ -> numberOfTests) $
        describe "Digit" $ do
          describe "numDigits" $ do
            it "numDigits works on numbers close to 0" $ do
              forAll digitsGen $ \x -> 
                (numDigits x) `shouldBe` 1

            it "numDigits works on arbitrarily large positive integers" $ do
              forAll withoutLeadingZero $ \x -> 
                (numDigits (listToInteger x)) `shouldBe` (length x)

          describe "sumDigits" $ do
            it "sumDigits works on numbers close to 0" $ do 
              forAll digitsGen $ \x -> 
                (sumDigits x) `shouldBe` x

            it "sumDigits works on arbitrarily large positive integers" $ do
              forAll withoutLeadingZero $ \x -> 
                (sumDigits (listToInteger x)) `shouldBe` (fromIntegral $ sum x)

          describe "toDigits" $ do 
            it "toDigits works on numbers close to 0" $ do
              forAll digitsGen $ \x -> 
                (toDigits x) `shouldBe` [x]

            it "toDigits works on arbitrarily large positive integers" $ do
              forAll withoutLeadingZero $ \x -> 
                (toDigits (listToInteger x)) `shouldBe` x

          describe "fromDigits" $ do
            it "fromDigits [] == 0" $ do
              (fromDigits []) `shouldBe` 0

            it "fromDigits works on lists starting with zero" $ do
              forAll withLeadingZero $ \xs ->
                (fromDigits xs) `shouldBe` (listToInteger xs)

            it "fromDigits works on lists of only zeros" $ do
              forAll zeroList $ \xs -> 
                (fromDigits xs) `shouldBe` 0

            it "fromDigits works on arbitrary lists" $ do
              forAll numbersGen $ \xs -> 
                (fromDigits xs) `shouldBe` (listToInteger xs)
