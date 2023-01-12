{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Math.NumberTheory.Fibonacci (spec) where

import Data.Mod (Mod, unMod)
import Data.Proxy (Proxy (Proxy), asProxyTypeOf)
import Math.NumberTheory.Fibonacci
  ( fibonacciN,
    fibonaccis,
    lucasNumN,
    lucasNums,
    lucasSeq,
    lucasSeqN,
  )
import System.Random (Random)
import System.Random.Stateful
  ( Uniform,
    UniformRange,
    uniformM,
    uniformRM,
    uniformWord32R,
  )
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, Property, choose, forAll, (.&&.))

instance Uniform (Mod 5) where
  uniformM g = do
    n <- uniformWord32R 4 g
    pure (fromIntegral n)

instance UniformRange (Mod 5) where
  uniformRM (l, u) g = do
    n <- uniformRM (unMod l, unMod u) g
    pure (fromIntegral n)

instance Random (Mod 5)

spec :: Spec
spec = do
  describe "fibonaccis" fibonaccisSpec
  describe "fibonacciN" fibonacciNSpec

  describe "lucasNums" lucasNumsSpec
  describe "lucasNumN" lucasNumNSpec

  describe "lucasSeq" lucasSeqSpec
  describe "lucasSeqN" lucasSeqNSpec

-- parameters
listLimit :: Integral a => a
listLimit = 10 ^ (2 :: Int)

seedLimit :: Int
seedLimit = 30

-- typeclass for comparing prefixes
class PrefixComparable a where
  comparePrefix :: Int -> a -> a -> Expectation

comparePrefix' :: PrefixComparable a => a -> a -> Expectation
comparePrefix' = comparePrefix listLimit

instance (Ord a, Show a) => PrefixComparable [a] where
  comparePrefix n xs ys = take n xs `shouldBe` take n ys

instance (PrefixComparable a) => PrefixComparable (a, a) where
  comparePrefix n xs ys = do
    comparePrefix n (fst xs) (fst ys)
    comparePrefix n (snd xs) (snd ys)

-- utilities
transpose :: [(a, a)] -> ([a], [a])
transpose xys = (map fst xys, map snd xys)

seedsGen :: (Num a, Random a) => Gen (a, a)
seedsGen = (,) <$> seedGen <*> seedGen
  where
    seedGen = choose (-fromIntegral seedLimit, fromIntegral seedLimit)

naiveLucas :: forall a. Num a => a -> a -> ([a], [a])
naiveLucas p q = (us, vs)
  where
    us :: [a]
    us = 0 : 1 : zipWith (-) (map (* p) (tail us)) (map (* q) us)
    vs :: [a]
    vs = 2 : p : zipWith (-) (map (* p) (tail vs)) (map (* q) vs)

-- test first listLimit against reference
fibonaccisSpec :: Spec
fibonaccisSpec =
  it ("first " ++ show listLimit ++ " correct") $
    let naiveFibonaccis :: [Integer]
        naiveFibonaccis = fst $ naiveLucas 1 (-1)
     in comparePrefix' fibonaccis naiveFibonaccis

-- test first listLimit against reference
fibonacciNSpec :: Spec
fibonacciNSpec =
  it ("first " ++ show listLimit ++ " correct") $
    let naiveFibonaccis :: [Integer]
        naiveFibonaccis = fst $ naiveLucas 1 (-1)
     in do
          comparePrefix' (map fibonacciN [(0 :: Int) ..]) naiveFibonaccis
          comparePrefix' (map fibonacciN [(0 :: Integer) ..]) naiveFibonaccis

-- test first listLimit against reference
lucasNumsSpec :: Spec
lucasNumsSpec =
  it ("first " ++ show listLimit ++ " correct") $
    let naiveLucasNums :: [Integer]
        naiveLucasNums = snd $ naiveLucas 1 (-1)
     in comparePrefix' lucasNums naiveLucasNums

-- test first listLimit against reference
lucasNumNSpec :: Spec
lucasNumNSpec =
  it ("first " ++ show listLimit ++ " correct") $
    let naiveLucasNums :: [Integer]
        naiveLucasNums = snd $ naiveLucas 1 (-1)
     in do
          comparePrefix' (map lucasNumN [(0 :: Int) ..]) naiveLucasNums
          comparePrefix' (map lucasNumN [(0 :: Integer) ..]) naiveLucasNums

-- check for random p and random q
-- check Int and Integer
lucasSeqSpec :: Spec
lucasSeqSpec =
  it "correct for arbitrary seeds" $
    let test ::
          (Num a, Random a, Show a, Ord a) =>
          Proxy (Gen (a, a)) ->
          Property
        test proxy =
          forAll (seedsGen `asProxyTypeOf` proxy) $ \(p, q) ->
            comparePrefix' (lucasSeq p q) (naiveLucas p q)
     in ((test @Int) Proxy)
          .&&. ((test @Integer) Proxy)
          .&&. ((test @(Mod 5) Proxy))

-- check for random p and random q, first listLimit
-- check Int and Integer
lucasSeqNSpec :: Spec
lucasSeqNSpec =
  it "correct for arbitrary seeds" $
    let test ::
          (Num a, Random a, Show a, Ord a) =>
          Proxy (Gen (a, a)) ->
          Property
        test proxy =
          forAll (seedsGen `asProxyTypeOf` proxy) $ \(p, q) -> do
            comparePrefix'
              (transpose $ map (lucasSeqN p q) [(0 :: Int) ..])
              (naiveLucas p q)
            comparePrefix'
              (transpose $ map (lucasSeqN p q) [(0 :: Integer) ..])
              (naiveLucas p q)
     in ((test @Int) Proxy)
          .&&. ((test @Integer) Proxy)
          .&&. ((test @(Mod 5) Proxy))
