module Hilbert.SquareSpec
  ( main
  , spec
  ) where

import Hilbert.Square (isSquare, integralSqrt)
import Hilbert.Digit (fromDigits)
import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.Int (Int16)

main :: IO ()
main = hspec spec

spec = modifyMaxSuccess (\_ -> numberOfTests) $
       describe "Square" $ do
         describe "isSquare" $ do
           smallCases_isSquare
           fixedPrecision_isSquare
           arbitraryPrecision_isSquare
           nonSquare
         describe "integralSqrt" $ do
           smallCases_integralSqrt
           fixedPrecision_integralSqrt
           arbitraryPrecision_integralSqrt

{-
   Parameters
-}
-- The number of tests to conduct of each type
numberOfTests = 500 :: Int

-- A generator to create very large numbers. First, the number of digits in the
-- test case is chosen at random from [1..maxNumDigits]. Then each of the
-- digits in the test case are generated using digitsGen.
numbersGen :: Gen Integer
numbersGen = do
    numDigits <- numDigitsGen
    list <- vectorOf numDigits digitsGen
    return (fromDigits list)
      where
        -- The generator for the number of digits in the test case
        numDigitsGen :: Gen Int
        numDigitsGen = choose (1, maxNumDigits)

        -- The maximum number of digits of any test case
        maxNumDigits = 1000 :: Int

        -- The generator for the digits to use in the tests
        digitsGen :: Gen Int
        digitsGen = choose (0, 9)

{-
   isSquare tests
-}
smallCases_isSquare = 
  it ("Finds all squares under " ++ (show (limit^2))) $ do
     (filter isSquare [0..(limit^2)]) `shouldBe` (map (^2) [0..limit])
  where limit = 100 :: Int

fixedPrecision_isSquare = 
  it "Works on fixed precision integers" $ do
    let bound = maxBound :: Int16
    let list1 = filter isSquare [1..bound]
    let list2 = map fromIntegral $ takeWhile (<= (fromIntegral bound))
                                 $ map (^2) [(1 :: Integer)..]
    list1 `shouldBe` list2

arbitraryPrecision_isSquare = 
  it "Works on arbitrarily sized squares" $ do
    forAll numbersGen $ \x ->
      (isSquare (x^2)) `shouldBe` True

nonSquare = 
  it "Filters out almost-squares" $ do
    forAll (suchThat numbersGen (> 2)) $ \x -> 
             [(isSquare (x^2 - 1)), (isSquare (x^2 + 1))]
               `shouldBe`
             [False, False]

{-
   integralSqrt tests
-}
smallCases_integralSqrt = 
  it "Works for all cases under 10000" $ do
    shouldBe True $ and $ map integralSqrt_def [1..10000]

fixedPrecision_integralSqrt = 
  it "Works on fixed precision integers" $ do
    let bound = maxBound :: Int16
    let list1 = map (\y -> (integralSqrt y, y)) [1..bound]
    let list2 = take (fromIntegral bound) $ concat $ map correctAnswer [1..]
          -- correctAnswer x is a list of all (x, y) such that
          -- integralSqrt y should equal x
          where correctAnswer :: Int16 -> [(Int16, Int16)]
                correctAnswer x = zip (repeat x) [x^2 .. (x + 1)^2 - 1]          
    sequence_ $ zipWith shouldBe list1 list2
    
arbitraryPrecision_integralSqrt = 
  it "Works on arbitrary precision integers" $ do
    forAll numbersGen integralSqrt_def

-- Checks the definition of integralSqrt n, ensuring that it returns
-- the largest integer less than the square root of n. This function is
-- used in the actual integralSqrt tests.
integralSqrt_def x = let s = integralSqrt x
                      in (s^2 <= x) && ((s + 1)^2 > x)
