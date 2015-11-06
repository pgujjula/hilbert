{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time.Clock

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
    start <- getCurrentTime
    let !r = fib 20
    end <- getCurrentTime
    putStrLn $ "fib 20 took " ++ show (diffUTCTime end start)
