{-# LANGUAGE TypeApplications #-}
module Math.Combinatorics.BinomialSpec (spec) where

import           Test.Hspec                  (Spec, describe, it, shouldBe)
import           Test.QuickCheck             (forAll, (===))
import qualified Test.QuickCheck             as QuickCheck (choose)

import           Math.Combinatorics.Binomial (factorial, choose, permute)

-- Limit for quickcheck inputs
limit :: Integer
limit = 1000

spec :: Spec
spec = do
    describe "factorial"  factorialSpec
    describe "choose"     chooseSpec
    describe "permute"    permuteSpec
    describe "invariants" invariantSpec

factorialSpec :: Spec
factorialSpec = do
    it "factorial 0"  $ factorial @Int 0 `shouldBe` 1
    it "factorial 1"  $ factorial @Int 1 `shouldBe` 1
    it "factorial 2"  $ factorial @Int 2 `shouldBe` 2
    it "factorial 10" $ factorial @Int 10 `shouldBe` 3628800
    it "factorial 50" $ factorial @Integer 50 `shouldBe`
        30414093201713378043612608166064768844377641568960512000000000000

chooseSpec :: Spec
chooseSpec = do
    it "out of bounds is zero" $ do
        choose (-1) 0 `shouldBe` 0
        choose 3 4 `shouldBe` 0
        choose 3 (-1) `shouldBe` 0
    it "small input" $
        map (5 `choose`) [0..5] `shouldBe` [1, 5, 10, 10, 5, 1]

permuteSpec :: Spec
permuteSpec = do
    it "out of bounds is zero" $ do
        permute(-1) 0 `shouldBe` 0
        permute 3 4 `shouldBe` 0
        permute 3 (-1) `shouldBe` 0
    it "small input" $
        map (5 `permute`) [0..5] `shouldBe` [1, 5, 20, 60, 120, 120]

invariantSpec :: Spec
invariantSpec =
    it "invariant: permute n k == choose n k * factorial k" $
        let gen = do
                n <- QuickCheck.choose (0, limit)
                k <- QuickCheck.choose (0, n)
                return (n, k)
         in forAll gen $ \(n, k) -> permute n k === choose n k * factorial k
