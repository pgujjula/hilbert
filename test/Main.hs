{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Test.Hspec (Spec)
import Test.Math.Combinatorics.Binomial qualified
import Test.Math.Combinatorics.Partition qualified
import Test.Math.NumberTheory.ContinuedFraction.Core qualified
import Test.Math.NumberTheory.ContinuedFraction.Sqrt qualified
import Test.Math.NumberTheory.Digit qualified
import Test.Math.NumberTheory.Diophantine qualified
import Test.Math.NumberTheory.Divisor qualified
import Test.Math.NumberTheory.Fibonacci qualified
import Test.Math.NumberTheory.Figurate qualified
import Test.Math.NumberTheory.Modular qualified
import Test.Math.NumberTheory.Power qualified
import Test.Math.NumberTheory.Prime qualified
import Test.Math.NumberTheory.Prime.Factor qualified
import Test.Math.Probability qualified
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = testGroup "Hilbert tests" <$> mapM (uncurry testSpec) specs

specs :: [(TestName, Spec)]
specs =
  [ ("Math.Combinatorics.Binomial", Test.Math.Combinatorics.Binomial.spec),
    ("Math.Combinatorics.Partitions", Test.Math.Combinatorics.Partition.spec),
    ("Math.NumberTheory.ContinuedFraction.Core", Test.Math.NumberTheory.ContinuedFraction.Core.spec),
    ("Math.NumberTheory.ContinuedFraction.Sqrt", Test.Math.NumberTheory.ContinuedFraction.Sqrt.spec),
    ("Math.NumberTheory.Digit", Test.Math.NumberTheory.Digit.spec),
    ("Math.NumberTheory.Diophantine", Test.Math.NumberTheory.Diophantine.spec),
    ("Math.NumberTheory.Divisor", Test.Math.NumberTheory.Divisor.spec),
    ("Math.NumberTheory.Fibonacci", Test.Math.NumberTheory.Fibonacci.spec),
    ("Math.NumberTheory.Figurate", Test.Math.NumberTheory.Figurate.spec),
    ("Math.NumberTheory.Modular", Test.Math.NumberTheory.Modular.spec),
    ("Math.NumberTheory.Power", Test.Math.NumberTheory.Power.spec),
    ("Math.NumberTheory.Prime", Test.Math.NumberTheory.Prime.spec),
    ("Math.NumberTheory.Prime.Factor", Test.Math.NumberTheory.Prime.Factor.spec),
    ("Math.Probability", Test.Math.Probability.spec)
  ]
