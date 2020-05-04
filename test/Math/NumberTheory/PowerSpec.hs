module Math.NumberTheory.PowerSpec (spec) where

import Control.Monad (forM_)
import Data.Int (Int16)
import System.Random (Random)

import Test.Hspec (Spec, shouldBe, it, describe)
import Test.QuickCheck (Gen, forAll, suchThat, choose, vectorOf, (===), arbitrary)
import Test.QuickCheck.Assertions ((<=?), (?>))

import Math.NumberTheory.Power (isSquare, integralSqrt, integralRoot, squares, cubes)
import Math.NumberTheory.Digit (fromDigits)

-- The maximum number of digits of any test case input
maxNumDigits :: Int
maxNumDigits = 1000

spec :: Spec
spec = do
    describe "squares" squareSpec
    describe "cubes" cubesSpec
    describe "isSquare" $ do
        smallCases_isSquare
        fixedPrecision_isSquare
        arbitraryPrecision_isSquare
        nonSquare
    describe "integralSqrt" $ do
        smallCases_integralSqrt
        fixedPrecision_integralSqrt
        arbitraryPrecision_integralSqrt
    describe "integralRoot" integralRootSpec

squareSpec :: Spec
squareSpec = do
    it "first 10 correct" $ take 10 squares `shouldBe` (map (^2) [0..9])

cubesSpec :: Spec
cubesSpec = do
    it "first 10 correct" $ take 10 cubes `shouldBe` (map (^3) [0..9])

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

        -- The generator for the digits to use in the tests
        digitsGen :: Gen Int
        digitsGen = choose (0, 9)

{-
   isSquare tests
-}
smallCases_isSquare = 
  it ("finds all squares under " ++ (show (limit^2))) $ do
     (filter isSquare [0..(limit^2)]) `shouldBe` (map (^2) [0..limit])
  where limit = 100 :: Int

fixedPrecision_isSquare = 
  it "works on fixed precision integers" $ do
    let bound = maxBound :: Int16
    let list1 = filter isSquare [1..bound]
    let list2 = map fromIntegral $ takeWhile (<= (fromIntegral bound))
                                 $ map (^2) [(1 :: Integer)..]
    list1 `shouldBe` list2

arbitraryPrecision_isSquare = 
  it "works on arbitrarily sized squares" $ do
    forAll numbersGen $ \x ->
      (isSquare (x^2)) `shouldBe` True

nonSquare = 
  it "filters out almost-squares" $ do
    forAll (suchThat numbersGen (> 2)) $ \x -> 
             [(isSquare (x^2 - 1)), (isSquare (x^2 + 1))]
               `shouldBe`
             [False, False]

{-
   integralSqrt tests
-}
smallCases_integralSqrt = 
  it "works for all cases under 10000" $ do
    shouldBe True $ and $ map integralSqrt_def [1..10000]

fixedPrecision_integralSqrt = 
  it "works on fixed precision integers" $ do
    let bound = maxBound :: Int16
    let list1 = map (\y -> (integralSqrt y, y)) [1..bound]
    let list2 = take (fromIntegral bound) $ concat $ map correctAnswer [1..]
           -- correctAnswer x is a list of all (x, y) such that
           -- integralSqrt y should equal x
           where correctAnswer :: Int16 -> [(Int16, Int16)]
                 correctAnswer y = map (\(a, b) -> (fromIntegral a, fromIntegral b))
                                 $ zip (repeat x) [x^2 .. (x + 1)^2 - 1]          
                    where x = toInteger y
    sequence_ $ zipWith shouldBe list1 list2
    
arbitraryPrecision_integralSqrt = 
  it "works on arbitrary precision integers" $ do
    forAll numbersGen integralSqrt_def

-- Checks the definition of integralSqrt n, ensuring that it returns
-- the largest integer less than the square root of n. This function is
-- used in the actual integralSqrt tests.
integralSqrt_def x = let s = integralSqrt x
                      in (s^2 <= x) && ((s + 1)^2 > x)

maxBase :: (Integral a) => a
maxBase = 1000

maxExponent :: (Integral a) => a
maxExponent = 30

integralRootSpec :: Spec
integralRootSpec = do
    it "base of 0" $ forM_ [1..5] $ \i -> integralRoot i 0 `shouldBe` 0
    it "base of 1" $ forM_ [0..5] $ \i -> integralRoot i 1 `shouldBe` 1
    it "exponent of 1" $
        forAll (choose (1, 1000 :: Int)) (\n -> integralRoot 1 n === n)

    let baseGen :: (Integral a, Random a) => Gen a
        baseGen = choose (2, maxBase)

        exponentGen :: (Integral a, Random a) => Gen a
        exponentGen = choose (2, maxExponent)

        powerGen :: Gen (Integer, Integer)
        powerGen = (,) <$> baseGen <*> exponentGen

    it "exact roots" $ forAll powerGen $ \(n, k) -> integralRoot k (n^k) === n
    it "one too big" $
        forAll powerGen $ \(n, k) -> integralRoot k (n^k + 1) === n
    it "one too small" $
        forAll powerGen $ \(n, k) -> integralRoot k (n^k - 1) === n - 1

    let intGen :: Gen Int
        intGen = arbitrary `suchThat` (> 0)

        fixedTestCase :: Gen (Int, Int)
        fixedTestCase = (,) <$> exponentGen <*> intGen

    it "fixed precision" $
        forAll fixedTestCase $ \(k, n) ->
            let root = integralRoot k n :: Int
             in root^k <= n && (toInteger root + 1)^k > (toInteger n)
