module Hilbert.DigitSpec (main, spec) where

import Hilbert.Digit (numDigits, sumDigits, digits)
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

-- The generator to create the actual test cases
numbersGen :: Gen [Int]
numbersGen = suchThat (numDigitsGen >>= (\x -> vectorOf x digitsGen))
                      ((/= 0) . head)

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
              forAll numbersGen $ \x -> 
                (numDigits (listToInteger x)) `shouldBe` (length x)

          describe "sumDigits" $ do
            it "sumDigits works on numbers close to 0" $ do 
              forAll digitsGen $ \x -> 
                (sumDigits x) `shouldBe` x

            it "sumDigits works on arbitrarily large positive integers" $ do
              forAll numbersGen $ \x -> 
                (sumDigits (listToInteger x)) `shouldBe` (fromIntegral $ sum x)

          describe "digits" $ do 
            it "digits works on numbers close to 0" $ do
              forAll digitsGen $ \x -> 
                (digits x) `shouldBe` [x]

            it "digits works on arbitrarily large positive integers" $ do
              forAll numbersGen $ \x -> 
                (digits (listToInteger x)) `shouldBe` x
