module Main where

import System.Exit (exitFailure, exitSuccess)
import System.Random

import Data.List (sortBy)

import Hilbert.ContinuedFrac (convergent, continuedFrac)
import Hilbert.Square (isSquare)

toInfinite :: (Integral a) => a -> [a]
toInfinite n = a:(cycle b)
  where Just (a, b) = continuedFrac n

test :: Integer -> Bool
test n = (reverse $ sortBy cmp list) == list
 where
  cmp :: Integer -> Integer -> Ordering
  cmp x y = compare (f x) (f y)
    where f x = abs $ (sqrt $ fromIntegral n) - (fromRational (convergent cfrac x))
          cfrac = toInfinite n
  list = take limit $ toInfinite n
  limit = 5

cases :: [Integer]
cases = filter (not . isSquare) [1..10]

wrongs :: [Integer]
wrongs = filter (not . test) cases

errorMsg :: Integer -> String
errorMsg n = 
  "ERROR: The continued fraction convergents for " ++ (show n)
  ++ " don't approach sqrt(" ++ (show n) ++ ").\n"

main = if null wrongs
       then exitSuccess
       else do
         sequence_ $ map (putStrLn . errorMsg) $ wrongs
         exitFailure
