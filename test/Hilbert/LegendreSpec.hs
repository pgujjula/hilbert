module Hilbert.LegendreSpec
  ( main
  , spec
  ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (property, Gen, elements, forAll, conjoin, Property, choose)
import Test.HUnit
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

import Debug.Trace (trace)
import Data.List (sort)
import qualified Data.Set as Set
import Data.Function (on)

import Hilbert.Legendre (legendre, jacobi)
import Hilbert.List (rmDups)
import Hilbert.Prime (primesTo, factor)

main :: IO ()
main = hspec spec

{-
   Parameters
-}
-- The number of tests to conduct of each type
numberOfTests = 500 :: Int

-- Upper bound for values in the tests
bound :: (Integral a) => a
bound = 10000

{-
   Tests
-}
spec = modifyMaxSuccess (\_ -> numberOfTests) $ do
         describe "legendre" $ do
            testMultiple
            modifyMaxSuccess (`div` 5) testResidue
            modifyMaxSuccess (`div` 5) testNonResidue
         describe "jacobi" $ do
            testAgainstLegendre

testMultiple =
    it "legendre a p == 0 if a is a multiple of p" $ 
        usingTester testPrime
    where testPrime :: Integer -> Expectation
          testPrime p = sequence_ $ zipWith shouldBe
                           (map (\n -> legendre n p) multiples)
                           (repeat 0)
             where numMultiples = 10
                   multiples = [0, p..(numMultiples - 1) * p]

testResidue = 
    it "legendre a p == -1 if a is a quadratic residue of p" $
        usingTester testPrime
    where testPrime :: Integer -> Expectation
          testPrime p = sequence_ $ zipWith shouldBe
                                (map (\n -> legendre n p) qrs)
                                (repeat 1)
            where qrs = quadraticResidues p

testNonResidue = 
    it "legendre a p == 1 if a is a quadratic residue of p" $
        usingTester testPrime
    where testPrime :: Integer -> Expectation
          testPrime p = sequence_ $ zipWith shouldBe
                                (map (\n -> legendre n p) nonqrs)
                                (repeat (-1))
            where qrs = quadraticResidues p
                  nonqrs = Set.toList $ (Set.difference `on` Set.fromList)
                                         [1..p - 1] qrs


testAgainstLegendre =  
    it "jacobi a n == product (legendre a p_i)^(k_i), where n = product (p_i)^(k_i)" $ do
        forAll pairGen $ \(a, n) ->
           (jacobi a n) `shouldBe` (jacobiFromLegendre a n)

{-
   Supplementary data/functions
-}
oddPrimeGen :: (Integral a) => Gen Integer
oddPrimeGen = elements $ drop 1 $ map toInteger $ primesTo bound

quadraticResidues :: (Integral a) => a -> [a]
quadraticResidues p = drop 1    -- drop the 0
                    $ rmDups $ sort
                    $ map (\x -> (x^2) `rem` p)
                        [0..p - 1]

pairGen :: Gen (Integer, Integer)
pairGen = do
    a <- choose (-bound, bound)
    n <- elements [1, 3..bound]
    return (a, n)

-- Apply a test on a single odd prime to many odd primes
usingTester :: (Integer -> Expectation) -> Property
usingTester testPrime = 
     conjoin -- Small cases
             [property (testPrime 3), 
              property (testPrime 5),
              property (testPrime 7),
             -- General case
              forAll oddPrimeGen testPrime]

jacobiFromLegendre :: Integer -> Integer -> Integer
jacobiFromLegendre a n = product $ map (\(p, k) -> (legendre a' p)^k) (factor n)
    where a' = a `mod` n
