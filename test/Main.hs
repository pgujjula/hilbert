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
      [pure Test.Math.Combinatorics.Binomial.tests,
       pure Test.Math.Combinatorics.Partition.tests,
       pure Test.Math.NumberTheory.ContinuedFraction.Core.tests,
       pure Test.Math.NumberTheory.ContinuedFraction.Sqrt.tests,
       pure Test.Math.NumberTheory.Digit.tests,
       pure Test.Math.NumberTheory.Diophantine.tests,
       pure Test.Math.NumberTheory.Divisor.tests,
       pure Test.Math.NumberTheory.Fibonacci.tests,
       pure Test.Math.NumberTheory.Figurate.tests,
       pure Test.Math.NumberTheory.Modular.tests,
       pure Test.Math.NumberTheory.Power.tests,
       Test.Math.NumberTheory.Prime.tests,
       Test.Math.NumberTheory.Prime.Factor.tests,
       pure Test.Math.Probability.tests
      ]
