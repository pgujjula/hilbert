module Hilbert.ListSpec
  ( main
  , spec
  ) where

import Hilbert.List (rmDups, groupBy)
import Test.Hspec (shouldBe, describe, hspec, it)
import Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import Test.QuickCheck (property, sample, choose, vector, Gen)
import Data.List (sort, sortBy)

sorted :: (Ord a) => [a] -> Bool
sorted (x1:x2:xs) = (x1 < x2) && (sorted (x2:xs))
sorted _ = True

numberOfTests = 500 :: Int
maxListSize = 500 :: Int

compareParity :: Int -> Int -> Int -> Ordering
compareParity n x y = compare (x `mod` n) (y `mod` n)

main = hspec spec
spec = modifyMaxSize (\_ -> maxListSize) $ 
         modifyMaxSuccess (\_ -> numberOfTests) $
         describe "List" $ do
           describe "rmDups" $ do
             it "works on small lists" $ do
               (rmDups [0, 0, 1, 2, 3, 3, 4]) `shouldBe` [0..4]
               (rmDups "ehllo") `shouldBe` "ehlo"
               (rmDups [1, 2, 3]) `shouldBe` [1, 2, 3]
             it "works on arbitrary lists" $ do
               property $ \x -> (sorted $ rmDups $ sort (x :: [Integer]))

           describe "groupBy" $ do
             it "Grouping integers by parity works" $ do
               property $ \xs -> 
                  (sort $ map sort $ groupBy (compareParity 2) xs)
                  `shouldBe` (sort $ map sort $ filter (not . null)
                              [filter even xs, filter odd xs])

             it "Grouping integers by equality works" $ do
               property $ \xs -> 
                  (sort $ map sort $ groupBy compare (xs :: [Int]))
                  `shouldBe` (sort $ map sort $ filter (not . null) $
                  map (\x -> filter (== x) xs) (rmDups $ sort xs))
