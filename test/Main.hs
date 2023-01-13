{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

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
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests =
  testGroup "Hilbert tests"
    <$> sequence
      [Test.Math.Combinatorics.Binomial.tests,
       pure Test.Math.Combinatorics.Partition.tests,
       Test.Math.NumberTheory.ContinuedFraction.Core.tests,
       Test.Math.NumberTheory.ContinuedFraction.Sqrt.tests,
       Test.Math.NumberTheory.Digit.tests,
       pure Test.Math.NumberTheory.Diophantine.tests,
       Test.Math.NumberTheory.Divisor.tests,
       Test.Math.NumberTheory.Fibonacci.tests,
       Test.Math.NumberTheory.Figurate.tests,
       Test.Math.NumberTheory.Modular.tests,
       Test.Math.NumberTheory.Power.tests,
       Test.Math.NumberTheory.Prime.tests,
       Test.Math.NumberTheory.Prime.Factor.tests,
       pure Test.Math.Probability.tests
      ]
