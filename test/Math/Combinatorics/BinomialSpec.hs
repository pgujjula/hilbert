{-# LANGUAGE TypeApplications #-}
module Math.Combinatorics.BinomialSpec (spec) where

import           Control.Exception           (evaluate)
import           Control.Monad               (forM_)
import           Test.Hspec                  (Spec, anyException, describe, it,
                                              shouldBe, shouldThrow)
import           Test.QuickCheck             (forAll, (===))
import qualified Test.QuickCheck             as QuickCheck (choose)

import           Math.Combinatorics.Binomial (binomialCoeffs, choose, factorial,
                                              permute, pascalDiagonal)

-- Limit for quickcheck inputs
limit :: Integer
limit = 1000

spec :: Spec
spec = do
    describe "factorial"      factorialSpec
    describe "choose"         chooseSpec
    describe "binomialCoeffs" binomialCoeffsSpec
    describe "permute"        permuteSpec
    describe "invariants"     invariantSpec
    describe "pascalDiagonal" pascalDiagonalSpec

factorialSpec :: Spec
factorialSpec = do
    it "factorial 0"  $ factorial @Int 0 `shouldBe` (1 :: Int)
    it "factorial 1"  $ factorial @Int 1 `shouldBe` (1 :: Int)
    it "factorial 2"  $ factorial @Int 2 `shouldBe` (2 :: Int)
    it "factorial 10" $ factorial @Int 10 `shouldBe` (3628800 :: Int)
    it "factorial 50" $ factorial @Integer 50 `shouldBe`
        (30414093201713378043612608166064768844377641568960512000000000000 :: Integer)

chooseSpec :: Spec
chooseSpec = do
    it "out of bounds is zero" $ do
        choose (-1) 0 `shouldBe` (0 :: Int)
        choose 3 4 `shouldBe` (0 :: Int)
        choose 3 (-1) `shouldBe` (0 :: Int)
    it "small input" $
        map (5 `choose`) [(0 :: Int)..5] `shouldBe` [1 :: Int, 5, 10, 10, 5, 1]

binomialCoeffsSpec :: Spec
binomialCoeffsSpec = do
    it "out of bounds throws an error" $ do
        evaluate (binomialCoeffs (-1 :: Int) :: [Int ]) `shouldThrow` anyException
        evaluate (binomialCoeffs (-5 :: Int) :: [Int]) `shouldThrow` anyException
    it "matches behavior of choose" $ do
        forM_ [(1 :: Int)..10] $ \n -> do
          (binomialCoeffs n :: [Int]) `shouldBe` fmap (n `choose`) [(0 :: Int)..n]

permuteSpec :: Spec
permuteSpec = do
    it "out of bounds is zero" $ do
        permute(-1) 0 `shouldBe` (0 :: Int)
        permute 3 4 `shouldBe` (0 :: Int)
        permute 3 (-1) `shouldBe` (0 :: Int)
    it "small input" $
        map (5 `permute`) [0..5] `shouldBe` [1 :: Int, 5, 20, 60, 120, 120]

invariantSpec :: Spec
invariantSpec =
    it "invariant: permute n k == choose n k * factorial k" $
        let gen = do
                n <- QuickCheck.choose (0, limit)
                k <- QuickCheck.choose (0, n)
                return (n, k)
         in forAll gen $ \(n, k) -> permute n k === choose n k * (factorial k :: Integer)

pascalDiagonalSpec :: Spec
pascalDiagonalSpec =
  it "equivalent to map (\\m -> (n+m) `choose` m) [0..]" $
    forM_ [0..(10 :: Int)] $ \n ->
      take 10 (pascalDiagonal n :: [Integer]) `shouldBe`
      take 10 (map (\m -> (n+m) `choose` m) [0..])
