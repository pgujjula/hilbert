module Hilbert.ContinuedFracSpec
  ( main
  , spec
  ) where

import Hilbert.ContinuedFrac (convergent, cfracSqrt)

import Hilbert.Square (isSquare)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Ratio
import Data.List (sort)
import Data.Maybe (fromJust)

main = hspec spec

numberOfTests :: Int
numberOfTests = 1000

spec = modifyMaxSuccess (\_ -> numberOfTests) 
     $ describe "ContinuedFrac" $ do
         it ("cfracSqrt computes the continued fraction representation of quadratic surds") $ do
           testCfracSqrt
         it ("Continued fraction convergents of sqrt n approach sqrt n (up to "
             ++ (show convergentLimit) ++ ")") $ do
           forAll nonSquareGen testConvergent

{---------------------------------------------------------------------
  Testing for cfracSqrt

  We compare the values of sqrt n computed by Hilbert with the values
  computed by Wolfram Alpha.
----------------------------------------------------------------------}

{-
   The continued fractions of sqrt(1), sqrt(2), ..., sqrt(10)
   courtesy of Wolfram Alpha.
-}
correctContinuedFrac :: [Maybe (Int, [Int])]
correctContinuedFrac =
               [Nothing,
                Just (1, [2]),
                Just (1, [1, 2]),
                Nothing,
                Just (2, [4]),
                Just (2, [2, 4]),
                Just (2, [1, 1, 1, 4]),
                Just (2, [1, 4]),
                Nothing,
                Just (3, [6])]

-- Test continued fractions for sqrt(1), sqrt(2), ..., sqrt(10)
testContinuedFrac :: [Maybe (Int, [Int])]
testContinuedFrac = map cfracSqrt [1..10]

testCfracSqrt = sequence_ $ zipWith shouldBe testContinuedFrac correctContinuedFrac

{-------------------------------------------------------------------------
                      Testing for convergent.

  We compute rational approximations for sqrt n using its continued
  fraction representation. We ensure that these rational approximations
  are monotonically decreasing and eventually equal sqrt n, up to machine
  precision.
---------------------------------------------------------------------------}

{-
   Compute the infinite continued fraction expansion
   for sqrt n.
-}
toInfinite :: (Integral a) => a -> [a]
toInfinite n = a:(cycle b)
  where Just (a, b) = cfracSqrt n

{-
   Get a list of improving rational approximations for sqrt n,
   using the continued fraction expansion of sqrt n.
-}
rationalApprox :: (Integral a) => a -> [Ratio a]
rationalApprox n = map convergent lists
  where cfrac = toInfinite n
        lists = map (\x -> take x cfrac) [1..]

{-
   A list of errors associated with approximating sqrt n using
   rational numbers. We stop computing errors once the approximation
   equals the actual value of sqrt n, up to machine precision.

   (rationalError n) !! i == abs $ n - ((rationalApprox n) !! i)^2
-}
rationalError :: (Integral a) => a -> [Ratio a]
rationalError n = take termsLimit $ map computeError (rationalApprox n)
  where computeError rat = abs $ (rat^2) - (n % 1)

nonSquareGen :: Gen Integer
nonSquareGen = elements $ filter (not . isSquare) [1..convergentLimit]

convergentLimit = 1000000
termsLimit = 10

isSorted :: (Ord a) => [a] -> Bool
isSorted xs = (sort xs) == xs

testConvergent :: (Integral a, Show a) => a -> Expectation
testConvergent n = (reverse list) `shouldBe` (sort list)
  where list = rationalError n
