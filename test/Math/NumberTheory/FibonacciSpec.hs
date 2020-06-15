{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
module Math.NumberTheory.FibonacciSpec (spec) where

import Data.Proxy (Proxy (Proxy), asProxyTypeOf)
import System.Random               (Random)
import Test.Hspec                  (Expectation, Spec, describe, it, shouldBe)
import Test.QuickCheck             (Gen, Property, choose, forAll, suchThat,
                                   (.&&.), oneof)

import Math.NumberTheory.Fibonacci (fibonacciModN, fibonacciN, fibonaccis,
                                    fibonaccisMod, lucasNumModN, lucasNumN,
                                    lucasNums, lucasNumsMod, lucasSeq,
                                    lucasSeqMod, lucasSeqModN, lucasSeqN)

spec :: Spec
spec = do
    describe "fibonaccis"    fibonaccisSpec
    describe "fibonaccisMod" fibonaccisModSpec
    describe "fibonacciN"    fibonacciNSpec
    describe "fibonacciModN" fibonacciModNSpec

    describe "lucasNums"     lucasNumsSpec
    describe "lucasNumsMod"  lucasNumsModSpec
    describe "lucasNumN"     lucasNumNSpec
    describe "lucasNumModN"  lucasNumModNSpec

    describe "lucasSeq"      lucasSeqSpec
    describe "lucasSeqMod"   lucasSeqModSpec
    describe "lucasSeqN"     lucasSeqNSpec
    describe "lucasSeqModN"  lucasSeqModNSpec

-- parameters
listLimit :: Integral a => a
listLimit = 10^(2 :: Int)

modLimit :: Int
modLimit = 10^(2 :: Int)

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

seedsGen :: (Integral a, Random a) => Gen (a, a)
seedsGen = (,) <$> seedGen <*> seedGen
  where
    seedGen = choose (-fromIntegral seedLimit, fromIntegral seedLimit)

modGen :: (Integral a, Random a) => Gen a
modGen = oneof [pure 1, choose (-modLimit', modLimit') `suchThat` (/= 0)]
  where
    modLimit' = fromIntegral modLimit

seedsModGen :: (Integral a, Random a) => Gen ((a, a), a)
seedsModGen = (,) <$> seedsGen <*> modGen

naiveLucas :: Integral a => Maybe a -> a -> a -> ([a], [a])
naiveLucas modulus p q = (map fromIntegral us, map fromIntegral vs)
  where
    us :: [Integer]
    us = reduce $ 0 : 1 : zipWith (-) (map (*p') (tail us)) (map (*q') us)
    vs :: [Integer]
    vs = reduce $ 2 : p' : zipWith (-) (map (*p') (tail vs)) (map (*q') vs)

    p' :: Integer
    p' = toInteger p

    q' :: Integer
    q' = toInteger q

    reduce :: [Integer] -> [Integer]
    reduce = case modulus of
                 Nothing -> id
                 Just m  -> map (`mod` abs (toInteger m))

--checkModular :: (Show a, Integral a) => a -> (a -> [a]) -> [a] -> Property
--checkModular m f ref =
--    take listLimit (f m) === take listLimit (map (`mod` m) ref)

-- test first listLimit against reference
fibonaccisSpec :: Spec
fibonaccisSpec = it ("first " ++ show listLimit ++ " correct") $
    let naiveFibonaccis :: [Integer]
        naiveFibonaccis = fst $ naiveLucas Nothing 1 (-1)
     in comparePrefix' fibonaccis naiveFibonaccis

-- test first listLimit against reference, for random modulus
-- test that modulus = 0 throws error
--
-- do above for @Int and @Integer
fibonaccisModSpec :: Spec
fibonaccisModSpec = it "correct for arbitrary modulus" $ 
    let test :: (Integral a, Random a, Show a) => Proxy (Gen a) -> Property
        test proxy = forAll (modGen `asProxyTypeOf` proxy) $ \m ->
            let naiveFibonaccis = fst $ naiveLucas (Just m) 1 (-1)
             in comparePrefix' (fibonaccisMod m) naiveFibonaccis
     in test (Proxy @(Gen Int)) .&&. test (Proxy @(Gen Integer))

-- test first listLimit against reference
fibonacciNSpec :: Spec
fibonacciNSpec = it ("first " ++ show listLimit ++ " correct") $
    let naiveFibonaccis :: [Integer]
        naiveFibonaccis = fst $ naiveLucas Nothing 1 (-1)
     in do
        comparePrefix' (map fibonacciN [(0 :: Int)..]) naiveFibonaccis
        comparePrefix' (map fibonacciN [(0 :: Integer)..]) naiveFibonaccis

-- test first listLimit against reference, for random modulus
-- test that modulus = 0 throws error
--
-- do above for @Int and @Integer
fibonacciModNSpec :: Spec
fibonacciModNSpec = it "correct for arbitrary modulus" $ 
    let test :: (Integral a, Random a, Show a) => Proxy (Gen a) -> Property
        test proxy = forAll (modGen `asProxyTypeOf` proxy) $ \m ->
            let naiveFibonaccis = fst $ naiveLucas (Just m) 1 (-1)
             in do
                comparePrefix' (map (fibonacciModN m) [(0 :: Int)..]) naiveFibonaccis
                comparePrefix' (map (fibonacciModN m) [(0 :: Integer)..]) naiveFibonaccis
     in test (Proxy @(Gen Int)) .&&. test (Proxy @(Gen Integer))

-- test first listLimit against reference
lucasNumsSpec :: Spec
lucasNumsSpec = it ("first " ++ show listLimit ++ " correct") $
    let naiveLucasNums :: [Integer]
        naiveLucasNums = snd $ naiveLucas Nothing 1 (-1)
     in comparePrefix' lucasNums naiveLucasNums

-- test first listLimit against reference, for random modulus
-- test that modulus = 0 throws error
--
-- do above for @Int and @Integer
lucasNumsModSpec :: Spec
lucasNumsModSpec = it "correct for arbitrary modulus" $ 
    let test :: (Integral a, Random a, Show a) => Proxy (Gen a) -> Property
        test proxy = forAll (modGen `asProxyTypeOf` proxy) $ \m ->
            let naiveLucasNums = snd $ naiveLucas (Just m) 1 (-1)
             in comparePrefix' (lucasNumsMod m) naiveLucasNums
     in test (Proxy @(Gen Int)) .&&. test (Proxy @(Gen Integer))

-- test first listLimit against reference
lucasNumNSpec :: Spec
lucasNumNSpec = it ("first " ++ show listLimit ++ " correct") $
    let naiveLucasNums :: [Integer]
        naiveLucasNums = snd $ naiveLucas Nothing 1 (-1)
     in do
        comparePrefix' (map lucasNumN [(0 :: Int)..]) naiveLucasNums
        comparePrefix' (map lucasNumN [(0 :: Integer)..]) naiveLucasNums

-- test first listLimit against reference, for random modulus
-- test that modulus = 0 throws error
--
-- do above for @Int and @Integer
lucasNumModNSpec :: Spec
lucasNumModNSpec = it "correct for arbitrary modulus" $
    let test :: (Integral a, Random a, Show a) => Proxy (Gen a) -> Property
        test proxy = forAll (modGen `asProxyTypeOf` proxy) $ \m ->
            let naiveLucasNums = snd $ naiveLucas (Just m) 1 (-1)
             in do
                comparePrefix' (map (lucasNumModN m) [(0 :: Int)..]) naiveLucasNums
                comparePrefix' (map (lucasNumModN m) [(0 :: Integer)..]) naiveLucasNums
     in test (Proxy @(Gen Int)) .&&. test (Proxy @(Gen Integer))

-- check for random p and random q
-- check Int and Integer
lucasSeqSpec :: Spec
lucasSeqSpec = it "correct for arbitrary seeds" $ 
    let test :: (Integral a, Random a, Show a)
             => Proxy (Gen (a, a))
             -> Property
        test proxy =
            forAll (seedsGen `asProxyTypeOf` proxy) $ \(p, q) ->
                comparePrefix' (lucasSeq p q) (naiveLucas Nothing p q)
     in ((test @Int) Proxy) .&&. ((test @Integer) Proxy)

lucasSeqModSpec :: Spec
lucasSeqModSpec = it "correct for arbitrary modulus and seeds" $ 
    let test :: (Integral a, Random a, Show a)
             => Proxy (Gen ((a, a), a))
             -> Property
        test proxy =
            forAll (seedsModGen `asProxyTypeOf` proxy) $ \((p, q), m) ->
                comparePrefix' (lucasSeqMod m p q) (naiveLucas (Just m) p q)
     in ((test @Int) Proxy) .&&. ((test @Integer) Proxy)

-- check for random p and random q, first listLimit
-- check Int and Integer
lucasSeqNSpec :: Spec
lucasSeqNSpec = it "correct for arbitrary seeds" $ 
    let test :: (Integral a, Random a, Show a)
             => Proxy (Gen (a, a))
             -> Property
        test proxy =
            forAll (seedsGen `asProxyTypeOf` proxy) $ \(p, q) -> do
                comparePrefix' (transpose $ map (lucasSeqN p q) [(0 :: Int)..])
                               (naiveLucas Nothing p q)
                comparePrefix' (transpose $ map (lucasSeqN p q) [(0 :: Integer)..])
                               (naiveLucas Nothing p q)
     in ((test @Int) Proxy) .&&. ((test @Integer) Proxy)

-- check for random p and random q and random modulus
-- for first listLimit
-- check Int and Integer
lucasSeqModNSpec :: Spec
lucasSeqModNSpec = it "correct for arbitrary modulus and seeds" $ 
    let test :: (Integral a, Random a, Show a)
             => Proxy (Gen ((a, a), a))
             -> Property
        test proxy =
            forAll (seedsModGen `asProxyTypeOf` proxy) $ \((p, q), m) -> do
                comparePrefix' (transpose $ map (lucasSeqModN m p q) [(0 :: Int)..])
                               (naiveLucas (Just m) p q)
                comparePrefix' (transpose $ map (lucasSeqModN m p q) [(0 :: Integer)..])
                               (naiveLucas (Just m) p q)
     in ((test @Int) Proxy) .&&. ((test @Integer) Proxy)
