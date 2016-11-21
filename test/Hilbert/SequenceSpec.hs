{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Hilbert.SequenceSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Control.Applicative (liftA2)
import System.Random (Random)
import Data.Int (Int8)

import Hilbert.Sequence
    (lucasU, lucasUAt, lucasUAtMod,
     lucasV, lucasVAt, lucasVAtMod)

main :: IO ()
main = hspec spec

{-
   Parameters
-}
numberOfTests = 500 :: Int

-- The bound on p and q, the parameters for the lucas sequences,
-- and on m, the modulus
pqmBound :: (Integral a, Random a) => a
pqmBound = 10000

-- How much of the beginning of the sequences to examine
sequenceLength :: Int
sequenceLength = 200

{-
   Tests
-}
spec = modifyMaxSuccess (\_ -> numberOfTests) $ do
          describe "lucasU" $ do
            it "satisfies initial conditions" $ do
                lucasU_initial

            it "satisfies recurrence relation" $ do 
                lucasU_recurrence
          
          describe "lucasUAt" $ do
            it "map lucasUAt [1..] matches output of lucasU" $ do
                lucasUAt_match

          describe "lucasUAtMod" $ do
            it "congruent to output of lucasUAt, in the range [0..modulus - 1]" $ do
                lucasUAtMod_match (pqmBound :: Integer)

            it "works with fixed precision inputs" $ do
                conjoin [
                    lucasUAtMod_fixed (undefined :: Int),
                    lucasUAtMod_fixed (undefined :: Int8)
                 ]

          describe "lucasV" $ do
            it "satisfies initial conditions" $ do
                lucasV_initial

            it "satisfies recurrence relation" $ do 
                lucasV_recurrence
          
          describe "lucasVAt" $ do
            it "map lucasVAt [1..] matches output of lucasV" $ do
                lucasVAt_match

          describe "lucasVAtMod" $ do
            it "congruent to output of lucasVAt, in the range [0..modulus - 1]" $ do
                lucasVAtMod_match (pqmBound :: Integer)

            it "works with fixed precision inputs" $ do
                conjoin [
                    lucasVAtMod_fixed (undefined :: Int),
                    lucasVAtMod_fixed (undefined :: Int8)
                 ]

lucasU_initial :: Property
lucasU_initial = conjoin [
    -- Fibonacci sequence
       property $ testPair (1, -1),
    -- General case
       forAll (pairGen pqmBound) testPair
    ]
        where testPair :: (Integer, Integer) -> Expectation
              testPair (p, q) = (take 2 $ lucasU p q) `shouldBe` [0, 1]

lucasU_recurrence :: Property
lucasU_recurrence = conjoin [
    -- Fibonacci sequence
    property $ testPair (1, -1),
    -- General case
    forAll (pairGen pqmBound) testPair
    ]
        where testPair :: (Integer, Integer) -> Expectation
              testPair (p, q) = sequence_ $ zipWith3 checkRecurrence
                                         sequence
                                         (drop 1 sequence)
                                         (drop 2 sequence)
                    where sequence = take sequenceLength $ lucasU p q
                          checkRecurrence a b c = c `shouldBe` (p*b - q*a)

lucasUAt_match :: Property
lucasUAt_match = conjoin [
    -- Fibonacci sequence
    property $ testPair (1, -1),
    -- General case
    forAll (pairGen pqmBound) testPair
    ]
        where testPair :: (Integer, Integer) -> Expectation
              testPair (p, q) = (map (lucasUAt p q) [0..limit])
                     `shouldBe` (take sequenceLength $ lucasU p q)
                where limit = toInteger $ sequenceLength - 1

lucasUAtMod_match :: forall a. (Integral a, Random a, Show a) => a -> Property 
lucasUAtMod_match bound = conjoin [
    -- Fibonacci sequence
    forAll (modGen bound) (\m -> testPair (1, -1, m)),
    -- Modulus is small
    forAll (pairGen bound) (\(a, b) -> testPair (a, b, 1)),
    -- General case
    forAll (tripleGen bound) testPair
    ]
        where testPair :: (a, a, a) -> Expectation
              testPair (p, q, m) = cmpMaps ((`mod` m) . (lucasUAt p q))
                                        (\n -> lucasUAtMod p q n m)
                                        [0..limit]

              limit :: a
              limit = fromIntegral $ sequenceLength - 1

lucasUAtMod_fixed :: forall a. (Integral a, Random a, Show a, Bounded a) => a -> Property 
lucasUAtMod_fixed _ = conjoin [
    -- Fibonacci sequence
    forAll (modGen (maxBound :: a)) (\m -> testPair (1, -1, m)),
    -- General case
    forAll (tripleGen (maxBound :: a)) testPair
    ]
        where testPair :: (a, a, a) -> Expectation
              testPair (p, q, m) = cmpMaps (\n -> toInteger $ lucasUAtMod p q n m)
                                           (\n -> lucasUAtMod (toInteger p) (toInteger q)
                                                              (toInteger n) (toInteger m))
                                           [1..limit]
              limit :: a
              limit = fromIntegral $ sequenceLength - 1

lucasV_initial :: Property
lucasV_initial = conjoin [
    -- Fibonacci sequence
       property $ testPair (1, -1),
    -- General case
       forAll (pairGen pqmBound) testPair
    ]
        where testPair :: (Integer, Integer) -> Expectation
              testPair (p, q) = (take 2 $ lucasV p q) `shouldBe` [2, p]

lucasV_recurrence :: Property
lucasV_recurrence = conjoin [
    -- Fibonacci sequence
    property $ testPair (1, -1),
    -- General case
    forAll (pairGen pqmBound) testPair
    ]
        where testPair :: (Integer, Integer) -> Expectation
              testPair (p, q) = sequence_ $ zipWith3 checkRecurrence
                                         sequence
                                         (drop 1 sequence)
                                         (drop 2 sequence)
                    where sequence = take sequenceLength $ lucasV p q
                          checkRecurrence a b c = c `shouldBe` (p*b - q*a)

lucasVAt_match :: Property
lucasVAt_match = conjoin [
    -- Fibonacci sequence
    property $ testPair (1, -1),
    -- General case
    forAll (pairGen pqmBound) testPair
    ]
        where testPair :: (Integer, Integer) -> Expectation
              testPair (p, q) = (map (lucasVAt p q) [0..limit])
                     `shouldBe` (take sequenceLength $ lucasV p q)
                where limit = toInteger $ sequenceLength - 1

lucasVAtMod_match :: forall a. (Integral a, Random a, Show a) => a -> Property 
lucasVAtMod_match bound = conjoin [
    -- Modulus is small
    forAll (pairGen bound) (\(a, b) -> testPair (a, b, 1)),
    -- Fibonacci sequence
    forAll (modGen bound) (\m -> testPair (1, -1, m)),
    -- General case
    forAll (tripleGen bound) testPair
    ]
        where testPair :: (a, a, a) -> Expectation
              testPair (p, q, m) = cmpMaps ((`mod` m) . (lucasVAt p q))
                                        (\n -> lucasVAtMod p q n m)
                                        [0..limit]

              limit :: a
              limit = fromIntegral $ sequenceLength - 1

lucasVAtMod_fixed :: forall a. (Integral a, Random a, Show a, Bounded a) => a -> Property 
lucasVAtMod_fixed _ = conjoin [
    -- Fibonacci sequence
    forAll (modGen (maxBound :: a)) (\m -> testPair (1, -1, m)),
    -- General case
    forAll (tripleGen (maxBound :: a)) testPair
    ]
        where testPair :: (a, a, a) -> Expectation
              testPair (p, q, m) = cmpMaps (\n -> toInteger $ lucasVAtMod p q n m)
                                           (\n -> lucasVAtMod (toInteger p) (toInteger q)
                                                              (toInteger n) (toInteger m))
                                           [1..limit]
              limit :: a
              limit = fromIntegral $ sequenceLength - 1

{-
   Supplementary functions/data
-}
excuse = pendingWith "Not implemented yet"

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair = liftA2 (,)

intGen :: (Integral a, Random a) => a -> Gen a
intGen bound = choose (-bound, bound)

realBound :: forall a. (Integral a, Random a, Bounded a) => a
realBound = if overflow
            then (maxBound :: a)
            else (pqmBound :: a)
    where overflow = (toInteger (pqmBound :: a)) /= pqmBound

pairGen :: (Integral a, Random a) => a -> Gen (a, a)
pairGen bound = genPair (intGen bound) (intGen bound)

modGen :: (Integral a, Random a) => a -> Gen a
modGen bound = choose (1, bound)

tripleGen :: (Integral a, Random a) => a -> Gen (a, a, a)
tripleGen bound = do
    m <- modGen bound
    (p, q) <- pairGen bound
    return (p, q, m)

cmpMaps :: (Eq b, Show b) => (a -> b) -> (a -> b) -> [a] -> Expectation
cmpMaps expFunc actFunc xs =
  sequence_ $ zipWith shouldBe actual expected
  where expected = map expFunc xs
        actual = map actFunc xs
