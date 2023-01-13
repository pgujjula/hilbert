{-# LANGUAGE MultiWayIf #-}

module Test.Math.NumberTheory.Prime.Factor (tests) where

import Control.Arrow ((>>>))
import Control.Monad (forM_, zipWithM_)
import Data.List.Duplicate (groupAdj)
import Data.Maybe (fromJust)
import Math.NumberTheory.Prime.Factor
  ( Factorization,
    factor,
    factorizations,
    factorizationsFrom,
    multiply,
    pow,
    simplify,
  )
import Math.NumberTheory.Prime.Sieve (primes)
import Test.QuickCheck (Gen, choose, forAll, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Prime.Factor"
    [ multiplyTest,
      powTest,
      simplifyTest,
      factorTest,
      factorizationsTest,
      factorizationsFromTest
    ]

limit :: (Integral a) => a
limit = 5000

powLimit :: Int
powLimit = 10

multiplyTest :: TestTree
multiplyTest =
  testGroup
    "multiply tests"
    [ testProperty "arbitrary inputs" $
        let gen :: Gen (Int, Int)
            gen = (,) <$> choose (1, limit) <*> choose (1, limit)
         in forAll gen $ \(x, y) ->
              (multiply <$> factor x <*> factor y) === factor (x * y)
    ]

powTest :: TestTree
powTest =
  testGroup
    "pow tests"
    [ testProperty "arbitrary inputs" $
        let gen :: Gen (Integer, Int)
            gen = (,) <$> choose (1, limit) <*> choose (1, powLimit)
         in forAll gen $ \(x, k) ->
              pow (fromJust $ factor x) k === fromJust (factor (x ^ k))
    ]

simplifyTest :: TestTree
simplifyTest =
  testGroup
    "simplify tests"
    [ testCase "empty list" $
        simplify [] @?= 1,
      testCase "small input" $ do
        simplify [(2, 3), (3, 1)] @?= 24
        simplify [(3, 1), (7, 1), (11, 1)] @?= 231
    ]

factorTest :: TestTree
factorTest =
  testGroup
    "factor tests"
    [ testCase "can't factor 0" $
        factor 0 @?= Nothing,
      testCase "correct up to limit" $
        zipWithM_ (@?=) (map (fromJust . factor) [1 .. limit]) factorizations
    ]

factorizationsTest :: TestTree
factorizationsTest =
  testGroup
    "factorizations tests"
    [ testCase "correct up to limit" $
        zipWithM_ (@?=) (take limit factorizations) (map factorNaive [1 .. limit])
    ]

factorizationsFromTest :: TestTree
factorizationsFromTest =
  testGroup
    "factorizationsFrom tests"
    [ testCase "correct up to limit, for many start points" $
        forM_ [1 .. 30] $ \i ->
          take limit (factorizationsFrom i)
            @?= take limit (drop (i - 1) factorizations)
    ]

factorNaive :: Int -> Factorization Int
factorNaive =
  findFactors
    >>> groupAdj
    >>> map (\x -> (head x, length x))

findFactors :: Int -> [Int]
findFactors n = findFactorsWith n (map fromIntegral primes)

findFactorsWith :: Int -> [Int] -> [Int]
findFactorsWith _ [] = error "impossible"
findFactorsWith n (p : ps) =
  let (q, r) = n `quotRem` p
   in if
          | p > n -> []
          | r == 0 -> p : findFactorsWith q (p : ps)
          | otherwise -> findFactorsWith n ps
