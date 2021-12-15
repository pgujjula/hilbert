module Math.NumberTheory.PowerSpec (spec) where

import Control.Monad           (forM_, zipWithM_)
import Data.Int                (Int16)
import Data.Maybe              (fromJust, isJust)
import Data.Proxy              (Proxy (..), asProxyTypeOf)
import Data.Word               (Word16)
import System.Random           (Random)

import Test.Hspec              (Expectation, Spec, describe, it, shouldBe,
                                shouldSatisfy)
import Test.QuickCheck         (Gen, arbitrary, choose, forAll, oneof, suchThat,
                                vectorOf, (===))

import Math.NumberTheory.Digit (fromDigits)
import Math.NumberTheory.Power (cube, cubes, integralLogBase, integralRoot,
                                integralSqrt, isSquare, square, squares)

-- The maximum number of digits of any test case input
maxNumDigits :: Int
maxNumDigits = 1000

spec :: Spec
spec = do
    describe "square" squareSpec
    describe "squares" squaresSpec

    describe "cube" cubeSpec
    describe "cubes" cubesSpec

    describe "isSquare" isSquareSpec
    describe "integralSqrt" integralSqrtSpec
    describe "integralRoot" integralRootSpec
    describe "integralLogBase" integralLogBaseSpec

listLimit :: Int
listLimit = 100

compareList :: (Eq a, Show a) => [a] -> [a] -> Expectation
compareList xs ys = take listLimit xs `shouldBe` take listLimit ys

squareSpec :: Spec
squareSpec =
    it ("first " ++ show listLimit ++ " correct") $ do
        compareList (fmap square [(0 :: Int)..]) (map (^2) [0..])
        compareList (fmap square [(0 :: Double)..]) (map (^2) [0..])

squaresSpec :: Spec
squaresSpec =
    it ("first " ++ show listLimit ++ " correct") $
        compareList squares (fmap (^2) [0..])

cubeSpec :: Spec
cubeSpec =
    it ("first " ++ show listLimit ++ " correct") $
        compareList (fmap cube [0..]) (fmap (^3) [0..])

cubesSpec :: Spec
cubesSpec =
    it ("first " ++ show listLimit ++ " correct") $
        take listLimit cubes `shouldBe` take listLimit (map (^3) [0..])

isSquareSpec :: Spec
isSquareSpec = do
    let limit = 100 :: Int
    it ("all squares under " ++ show (limit^2)) $
        filter isSquare [0..(limit^2)] `shouldBe` map (^2) [0..limit]

    it "fixed precision integers" $ do
        let f proxy =
              let bound = maxBound `asProxyTypeOf` proxy
                  actual = filter isSquare [1..bound]
                  expected = fmap fromIntegral
                    $ takeWhile (<= fromIntegral bound)
                    $ map (^2) [(1 :: Integer)..]
               in actual `shouldBe` expected
        f (Proxy :: Proxy Int16)
        f (Proxy :: Proxy Word16)

    it "exact squares" $
        forAll numbersGen $ \x -> isSquare (x^2)

    it "one too small" $
        forAll (suchThat numbersGen (> 2)) $ \x ->
            not $ isSquare (x^2 - 1)

    it "one too large" $
        forAll (suchThat numbersGen (> 2)) $ \x ->
            not $ isSquare (x^2 + 1)

-- A generator to create very large numbers. First, the number of digits in the
-- test case is chosen at random from [1..maxNumDigits], and then a random number
-- is generated with that many digits
numbersGen :: Gen Integer
numbersGen = do
    numDigits <- choose (1, maxNumDigits)
    list <- vectorOf numDigits (choose (0, 9))
    return (fromDigits list)

{-
   integralSqrt tests
-}
integralSqrtSpec :: Spec
integralSqrtSpec = do
    let check x = let s = integralSqrt x
                  in (s^2 <= x) && ((s + 1)^2 > x)

    let limit = 10000 :: Int
    it ("all integers under " ++ show limit) $
        mapM_ (`shouldSatisfy` check) [1..limit]

    it "fixed precision" $ do
        let bound = maxBound :: Int16
        let actual = map integralSqrt [1..bound]
        let expected = map (fromIntegral . integralSqrt) [1..toInteger bound]
        zipWithM_ shouldBe actual expected

    it "arbitrary precision" $
        forAll numbersGen check

    it "perfect squares" $
        forAll numbersGen (\x -> integralSqrt (x^2) === x)

    it "one too small" $
        forAll (numbersGen `suchThat` (> 0)) $ \x ->
            integralSqrt (x^2 - 1) === (x - 1)

    it "one too large" $
        forAll (numbersGen `suchThat` (> 0)) $ \x ->
            integralSqrt (x^2 + 1) === x

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
             in root^k <= n && (toInteger root + 1)^k > toInteger n

integralLogBaseSpec :: Spec
integralLogBaseSpec = do
    it "negative base yields Nothing" $ do
        integralLogBase (-2) 3 `shouldBe` Nothing
        integralLogBase (-5) 3 `shouldBe` Nothing
    it "zero base yields Nothing" $
        integralLogBase 0 3 `shouldBe` Nothing
    it "base of 1 yields Nothing" $
        integralLogBase 1 3 `shouldBe` Nothing
    it "negative input yields Nothing" $ do
        integralLogBase 3 (-5) `shouldBe` Nothing
        integralLogBase 3 (-2) `shouldBe` Nothing
    it "zero input yields Nothing" $
        integralLogBase 3 0 `shouldBe` Nothing

    it "valid for general input" $ do
        let baseGen :: Gen Integer
            baseGen = choose (2, 10)

        let inputGen :: Gen Integer
            inputGen = oneof [choose (1, 10), choose (1, 1000), choose (1, 10^100)]

        forAll ((,) <$> baseGen <*> inputGen) $ \(b, n) -> do
            let maybeE = integralLogBase b n
            maybeE `shouldSatisfy` isJust
            let e = fromJust maybeE
            b^e `shouldSatisfy` (<= n)
            b^(e + 1) `shouldSatisfy` (> n)
