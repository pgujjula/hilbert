module Hilbert.ListSpec
  ( main
  , spec
  ) where

import Hilbert.List (rmDups, groupBy)
import Test.Hspec (shouldBe, describe, hspec, it)
import Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import Test.QuickCheck (property, sample, choose, vector, Gen)
import Data.List (sort, sortBy)

main :: IO ()
main = hspec spec

spec = modifyMaxSize (\_ -> maxListSize) $ 
         modifyMaxSuccess (\_ -> numberOfTests) $
           describe "List" $ do
             describe "rmDups" $ do
               smallCase_rmDups
               arbitraryCase_rmDups
             describe "groupBy" $ do
               parity_groupBy
               equality_groupBy
{- 
   Parameters and supplementary functions
-}
-- True if a list is sorted
sorted :: (Ord a) => [a] -> Bool
sorted (x1:x2:xs) = (x1 < x2) && (sorted (x2:xs))
sorted _ = True

-- Compare two numbers modulo n
compareParity :: Int -> Int -> Int -> Ordering
compareParity n x y = compare (x `mod` n) (y `mod` n)

numberOfTests = 500 :: Int
maxListSize = 500 :: Int

{-
   rmDups tests
-}
smallCase_rmDups = 
  it "works on small lists" $ do
    (rmDups [0, 0, 1, 2, 3, 3, 4]) `shouldBe` [0..4]
    (rmDups "ehllo") `shouldBe` "ehlo"
    (rmDups [1, 2, 3]) `shouldBe` [1, 2, 3]

arbitraryCase_rmDups =
  it "works on arbitrary lists" $ do
    property $ \x -> (sorted $ rmDups $ sort (x :: [Integer]))

{- 
   groupBy tests
-}
parity_groupBy = 
  it "Grouping integers by parity works" $ do
    property $ \xs -> 
      (sort $ map sort $ groupBy (compareParity 2) xs)
      `shouldBe` (sort $ map sort $ filter (not . null)
                  [filter even xs, filter odd xs])

equality_groupBy = 
  it "Grouping integers by equality works" $ do
    property $ \xs -> 
      (sort $ map sort $ groupBy compare (xs :: [Int])) 
        `shouldBe`
      (sort $ map sort $ filter (not . null) $
        map (\x -> filter (== x) xs) (rmDups $ sort xs))
