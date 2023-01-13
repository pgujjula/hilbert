{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Math.NumberTheory.Fibonacci (tests) where

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
import Test.HUnit (Assertion)
import Test.QuickCheck (Gen, Property, choose, forAll, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

instance Uniform (Mod 5) where
  uniformM g = do
    n <- uniformWord32R 4 g
    pure (fromIntegral n)

instance UniformRange (Mod 5) where
  uniformRM (l, u) g = do
    n <- uniformRM (unMod l, unMod u) g
    pure (fromIntegral n)

instance Random (Mod 5)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Fibonacci"
    [ fibonaccisTest,
      fibonacciNTest,
      lucasNumsTest,
      lucasNumNTest,
      lucasSeqTest,
      lucasSeqNTest
    ]

-- parameters
listLimit :: Integral a => a
listLimit = 10 ^ (2 :: Int)

seedLimit :: Int
seedLimit = 30

-- typeclass for comparing prefixes
class PrefixComparable a where
  comparePrefix :: Int -> a -> a -> Property
  comparePrefixM :: Int -> a -> a -> Assertion

comparePrefixM' :: PrefixComparable a => a -> a -> Assertion
comparePrefixM' = comparePrefixM listLimit

comparePrefix' :: PrefixComparable a => a -> a -> Property
comparePrefix' = comparePrefix listLimit

instance (Ord a, Show a) => PrefixComparable [a] where
  comparePrefix n xs ys = take n xs === take n ys
  comparePrefixM n xs ys = take n xs @?= take n ys

instance (PrefixComparable a) => PrefixComparable (a, a) where
  comparePrefix n xs ys =
    comparePrefix n (fst xs) (fst ys)
      .&&. comparePrefix n (snd xs) (snd ys)

  comparePrefixM n xs ys = do
    comparePrefixM n (fst xs) (fst ys)
    comparePrefixM n (snd xs) (snd ys)

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
fibonaccisTest :: TestTree
fibonaccisTest =
  testGroup
    "fibonaccis tests"
    [ testCase ("first " ++ show listLimit ++ " correct") $
        let naiveFibonaccis :: [Integer]
            naiveFibonaccis = fst $ naiveLucas 1 (-1)
         in comparePrefixM' fibonaccis naiveFibonaccis
    ]

-- test first listLimit against reference
fibonacciNTest :: TestTree
fibonacciNTest =
  testGroup
    "fibonacciN tests"
    [ testCase ("first " ++ show listLimit ++ " correct") $
        let naiveFibonaccis :: [Integer]
            naiveFibonaccis = fst $ naiveLucas 1 (-1)
         in do
              comparePrefixM' (map fibonacciN [(0 :: Int) ..]) naiveFibonaccis
              comparePrefixM' (map fibonacciN [(0 :: Integer) ..]) naiveFibonaccis
    ]

-- test first listLimit against reference
lucasNumsTest :: TestTree
lucasNumsTest =
  testGroup
    "lucasNums tests"
    [ testCase ("first " ++ show listLimit ++ " correct") $
        let naiveLucasNums :: [Integer]
            naiveLucasNums = snd $ naiveLucas 1 (-1)
         in comparePrefixM' lucasNums naiveLucasNums
    ]

-- test first listLimit against reference
lucasNumNTest :: TestTree
lucasNumNTest =
  testGroup
    "lucasNumN tests"
    [ testCase ("first " ++ show listLimit ++ " correct") $
        let naiveLucasNums :: [Integer]
            naiveLucasNums = snd $ naiveLucas 1 (-1)
         in do
              comparePrefixM' (map lucasNumN [(0 :: Int) ..]) naiveLucasNums
              comparePrefixM' (map lucasNumN [(0 :: Integer) ..]) naiveLucasNums
    ]

-- check for random p and random q
-- check Int and Integer
lucasSeqTest :: TestTree
lucasSeqTest =
  testGroup
    "lucasSeq test"
    [ testProperty "correct for arbitrary seeds" $
        let test ::
              (Num a, Random a, Show a, Ord a) =>
              Proxy (Gen (a, a)) ->
              Property
            test proxy =
              forAll (seedsGen `asProxyTypeOf` proxy) $ \(p, q) ->
                comparePrefix' (lucasSeq p q) (naiveLucas p q)
         in test @Int Proxy
              .&&. test @Integer Proxy
              .&&. test @(Mod 5) Proxy
    ]

-- check for random p and random q, first listLimit
-- check Int and Integer
lucasSeqNTest :: TestTree
lucasSeqNTest =
  testGroup
    "lucasSeqN tests"
    [ testProperty "correct for arbitrary seeds" $
        let test ::
              (Num a, Random a, Show a, Ord a) =>
              Proxy (Gen (a, a)) ->
              Property
            test proxy =
              forAll (seedsGen `asProxyTypeOf` proxy) $ \(p, q) ->
                comparePrefix'
                  (transpose $ map (lucasSeqN p q) [(0 :: Int) ..])
                  (naiveLucas p q)
                  .&&. comparePrefix'
                    (transpose $ map (lucasSeqN p q) [(0 :: Integer) ..])
                    (naiveLucas p q)
         in test @Int Proxy
              .&&. test @Integer Proxy
              .&&. test @(Mod 5) Proxy
    ]
