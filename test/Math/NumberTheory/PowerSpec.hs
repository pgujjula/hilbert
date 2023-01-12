module Math.NumberTheory.PowerSpec (spec) where

import Data.Maybe (fromJust, isJust)
import Math.NumberTheory.Power
  ( cube,
    cubes,
    integralLogBase,
    square,
    squares,
  )
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Test.QuickCheck
  ( Gen,
    choose,
    forAll,
    oneof,
  )

spec :: Spec
spec = do
  describe "square" squareSpec
  describe "squares" squaresSpec

  describe "cube" cubeSpec
  describe "cubes" cubesSpec

  describe "integralLogBase" integralLogBaseSpec

listLimit :: Int
listLimit = 100

compareList :: (Eq a, Show a) => [a] -> [a] -> Expectation
compareList xs ys = take listLimit xs `shouldBe` take listLimit ys

squareSpec :: Spec
squareSpec =
  it ("first " ++ show listLimit ++ " correct") $ do
    compareList (fmap square [(0 :: Int) ..]) (map (^ 2) [0 ..])
    compareList (fmap square [(0 :: Double) ..]) (map (^ 2) [0 ..])

squaresSpec :: Spec
squaresSpec =
  it ("first " ++ show listLimit ++ " correct") $
    compareList squares (fmap (^ 2) [0 ..])

cubeSpec :: Spec
cubeSpec =
  it ("first " ++ show listLimit ++ " correct") $
    compareList (fmap cube [0 ..]) (fmap (^ 3) [0 ..])

cubesSpec :: Spec
cubesSpec =
  it ("first " ++ show listLimit ++ " correct") $
    take listLimit cubes `shouldBe` take listLimit (map (^ 3) [0 ..])

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
        inputGen = oneof [choose (1, 10), choose (1, 1000), choose (1, 10 ^ 100)]

    forAll ((,) <$> baseGen <*> inputGen) $ \(b, n) -> do
      let maybeE = integralLogBase b n
      maybeE `shouldSatisfy` isJust
      let e = fromJust maybeE
      b ^ e `shouldSatisfy` (<= n)
      b ^ (e + 1) `shouldSatisfy` (> n)
