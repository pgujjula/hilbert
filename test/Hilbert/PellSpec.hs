module Hilbert.PellSpec
  ( main
  , spec
  ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Maybe (fromJust)

import Debug.Trace (trace)
import Hilbert.Square (isSquare)
import Hilbert.Pell (solve)


main :: IO ()
main = hspec spec

spec = modifyMaxSuccess (\_ -> numberOfTests) $ 
         describe "solve" $ do
           it "solves the Pell equation" $ do
             test_solutions

test_solutions = forAll nonSquareGen testWith

{-
   Supplementary data/functions
-}
numberOfTests :: Int
numberOfTests = 500

testSizeLimit :: Integer
testSizeLimit = 1000

solutionLimit :: Int
solutionLimit = 100

nonSquareGen :: Gen Integer
nonSquareGen = elements $ filter (not . isSquare) [1..testSizeLimit]

testWith d = sequence_ $ map checkSolution
           $ take solutionLimit $ fromJust $ solve d
  where checkSolution (x, y) = (x^2 - d * y^2) `shouldBe` 1
