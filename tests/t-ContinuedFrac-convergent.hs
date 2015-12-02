module Main where

import System.Exit (exitFailure, exitSuccess)
import System.Random

import Data.List (sort)

import Hilbert.ContinuedFrac (convergent, continuedFrac)
import Hilbert.Square (isSquare)

toInfinite :: (Integral a) => a -> [a]
toInfinite n = a:(cycle b)
  where Just (a, b) = continuedFrac n

test n = (reverse $ sort approximations) == approximations
 where
  list = [1..limit]
  limit = 5
  Just (a, b) = continuedFrac n
  cfrac = a:(cycle b)
  approximations = map (abs . ((sqrt (fromIntegral n)) - ) . fromRational
                          . (convergent cfrac)) list


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
