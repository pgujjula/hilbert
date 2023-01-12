{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Test.Math.Combinatorics.Binomial (tests,spec) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Mod (Mod)
import Data.Type.Natural (SNat, withSNat)
import Math.Combinatorics.Binomial
  ( binomialCoeffs,
    choose,
    chooseModP,
    chooseModP2,
    factorial,
    factorialModP,
    factorialNoPModP,
    factorialNoPModP2,
    factorialRelPrimeModP,
    factorialRelPrimeModP2,
    mkFactorialModP,
    mkFactorialModP2,
    pascalDiagonal,
    permute,
  )
import Math.NumberTheory.Divisor (divides)
import Math.NumberTheory.Prime (primesTo)
import Test.Hspec
  ( Spec,
    anyException,
    describe,
    it,
    shouldBe,
    shouldThrow,
  )
import Test.QuickCheck (forAll, (===))
import Test.QuickCheck qualified as QuickCheck (choose)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

-- Limit for quickcheck inputs
limit :: Integer
limit = 1000

tests :: IO TestTree
tests = testSpec "Math.Combinatorics.Binomial" spec

spec :: Spec
spec = do
  describe "factorial" factorialSpec
  describe "choose" chooseSpec
  describe "binomialCoeffs" binomialCoeffsSpec
  describe "permute" permuteSpec
  describe "invariants" invariantSpec
  describe "pascalDiagonal" pascalDiagonalSpec
  describe "factorialModP" factorialModPSpec
  describe "factorialRelPrimeModP" factorialRelPrimeModPSpec
  describe "factorialNoPModP" factorialNoPModPSpec
  describe "chooseModP" chooseModPSpec
  describe "factorialRelPrimeModP2" factorialRelPrimeModP2Spec
  describe "factorialNoPModP2" factorialNoPModP2Spec
  describe "chooseModP2" chooseModP2Spec
  describe "factorialRelPrimeModP2" factorialRelPrimeModP2Spec

factorialSpec :: Spec
factorialSpec = do
  it "factorial 0" $ factorial @Int 0 `shouldBe` (1 :: Int)
  it "factorial 1" $ factorial @Int 1 `shouldBe` (1 :: Int)
  it "factorial 2" $ factorial @Int 2 `shouldBe` (2 :: Int)
  it "factorial 10" $ factorial @Int 10 `shouldBe` (3628800 :: Int)
  it "factorial 50" $
    factorial @Integer 50
      `shouldBe` (30414093201713378043612608166064768844377641568960512000000000000 :: Integer)

chooseSpec :: Spec
chooseSpec = do
  it "out of bounds is zero" $ do
    choose (-1) 0 `shouldBe` (0 :: Int)
    choose 3 4 `shouldBe` (0 :: Int)
    choose 3 (-1) `shouldBe` (0 :: Int)
  it "small input" $
    map (5 `choose`) [(0 :: Int) .. 5] `shouldBe` [1 :: Int, 5, 10, 10, 5, 1]

binomialCoeffsSpec :: Spec
binomialCoeffsSpec = do
  it "out of bounds throws an error" $ do
    evaluate (binomialCoeffs (-1 :: Int) :: [Int]) `shouldThrow` anyException
    evaluate (binomialCoeffs (-5 :: Int) :: [Int]) `shouldThrow` anyException
  it "matches behavior of choose" $ do
    forM_ [(1 :: Int) .. 10] $ \n -> do
      (binomialCoeffs n :: [Int]) `shouldBe` fmap (n `choose`) [(0 :: Int) .. n]

permuteSpec :: Spec
permuteSpec = do
  it "out of bounds is zero" $ do
    permute (-1) 0 `shouldBe` (0 :: Int)
    permute 3 4 `shouldBe` (0 :: Int)
    permute 3 (-1) `shouldBe` (0 :: Int)
  it "small input" $
    map (5 `permute`) [0 .. 5] `shouldBe` [1 :: Int, 5, 20, 60, 120, 120]

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
    forM_ [0 .. (10 :: Int)] $ \n ->
      take 10 (pascalDiagonal n :: [Integer])
        `shouldBe` take 10 (map (\m -> (n + m) `choose` m) [0 ..])

factorialModPSpec :: Spec
factorialModPSpec =
  it "equivalent to n! (mod p)" $
    forM_ (primesTo 100) $ \p ->
      withSNat (fromIntegral p) $ \(_ :: SNat p) ->
        let cmod = mkFactorialModP @p
         in forM_ [0 .. 100] $ \k ->
              factorialModP cmod k `shouldBe` (factorial k :: Mod p)

factorialRelPrimeModPSpec :: Spec
factorialRelPrimeModPSpec =
  it "works for small n, p" $
    forM_ (map toInteger (primesTo 100)) $ \p ->
      withSNat (fromIntegral p) $ \(_ :: SNat p) ->
        let cmod = mkFactorialModP @p
         in forM_ [1 .. (100 :: Integer)] $ \k ->
              let expected =
                    fromIntegral $
                      product (filter (not . (p `divides`)) [1 .. k])
               in factorialRelPrimeModP cmod k `shouldBe` expected

divideOut :: Integral a => a -> a -> a
divideOut n p =
  case quotRem n p of
    (q, 0) -> divideOut q p
    _ -> n

factorialNoPModPSpec :: Spec
factorialNoPModPSpec =
  it "works for small n, p" $
    forM_ (map toInteger (primesTo 100)) $ \p ->
      withSNat (fromIntegral p) $ \(_ :: SNat p) ->
        let cmod = mkFactorialModP @p
         in forM_ [1 .. (100 :: Integer)] $ \k ->
              let expected =
                    fromIntegral $
                      (factorial k `divideOut` p)
               in (k, p, factorialNoPModP cmod k)
                    `shouldBe` (k, p, expected)

chooseModPSpec :: Spec
chooseModPSpec =
  it "works for small n, m, p" $
    forM_ (map toInteger (primesTo 100)) $ \p ->
      withSNat (fromIntegral p) $ \(_ :: SNat p) ->
        let fmp = mkFactorialModP @p
         in forM_ [1 .. (30 :: Integer)] $ \n ->
              forM_ [1 .. (30 :: Integer)] $ \m ->
                let expected = fromIntegral ((n `choose` m) :: Integer)
                    actual = chooseModP fmp n m
                 in actual `shouldBe` expected

factorialRelPrimeModP2Spec :: Spec
factorialRelPrimeModP2Spec =
  it "works for small n, p" $
    forM_ (map toInteger (primesTo 100)) $ \p ->
      withSNat (fromIntegral p) $ \(_ :: SNat p) ->
        let fmp2 = mkFactorialModP2 @p
         in forM_ [1 .. (100 :: Integer)] $ \n ->
              let actual = factorialRelPrimeModP2 fmp2 n
                  expected =
                    fromIntegral $
                      [1 .. n]
                        & filter (not . (p `divides`))
                        & product
               in (n, p, actual) `shouldBe` (n, p, expected)

factorialNoPModP2Spec :: Spec
factorialNoPModP2Spec =
  it "works for small n, p" $
    forM_ (map toInteger (primesTo 100)) $ \p ->
      withSNat (fromIntegral p) $ \(_ :: SNat p) ->
        let fmp2 = mkFactorialModP2 @p
         in forM_ [1 .. (100 :: Integer)] $ \n ->
              let actual = factorialNoPModP2 fmp2 n
                  expected =
                    fromIntegral $
                      [1 .. n]
                        & product
                        & (`divideOut` p)
               in (n, p, actual) `shouldBe` (n, p, expected)

chooseModP2Spec :: Spec
chooseModP2Spec =
  it "works for small n, m, p" $
    forM_ (map toInteger (primesTo 10)) $ \p ->
      withSNat (fromIntegral p) $ \(_ :: SNat p) ->
        let fmp = mkFactorialModP2 @p
         in forM_ [1 .. (100 :: Integer)] $ \n ->
              forM_ [1 .. (100 :: Integer)] $ \m ->
                let expected = fromIntegral ((n `choose` m) :: Integer)
                    actual = chooseModP2 fmp n m
                 in actual `shouldBe` expected
