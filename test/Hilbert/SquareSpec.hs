module Hilbert.SquareSpec (main, spec) where

import Hilbert.Square (isSquare)
import Hilbert.Digit (fromDigits)
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

-- The number of tests to conduct of each type
numberOfTests = 500 :: Int

-- The maximum number of digits to use in test cases
maxNumDigits = 1000 :: Int

-- The generator to create the actual test cases
numbersGen :: Gen Integer
numbersGen = do
    numDigits <- numDigitsGen
    list <- vectorOf numDigits digitsGen
    return (fromDigits list)
      where
        -- The generator for the number of digits in the test case
        numDigitsGen :: Gen Int
        numDigitsGen = choose (1, maxNumDigits)

        -- The generator for the digits to use in the tests
        digitsGen :: Gen Int
        digitsGen = choose (0, 9)

spec = modifyMaxSuccess (\_ -> numberOfTests) $
       describe "Square" $ do
         describe "isSquare" $ do
           let limit = 100
           it ("Correctly finds all squares under " ++ (show (limit^2))) $ do
             (filter isSquare [0..(limit^2)]) `shouldBe` (map (^2) [0..limit])

           it "Correctly works on arbitrarily sized integers" $ do
             forAll numbersGen $ \x ->
               (isSquare (x^2)) `shouldBe` True

           it "Correctly filters out non-squares" $ do
             forAll (suchThat numbersGen (> 2)) $ \x -> 
                      [(isSquare (x^2 - 1)), (isSquare (x^2 + 1))]
                        `shouldBe`
                      [False, False]
