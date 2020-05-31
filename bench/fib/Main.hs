module Main (main) where

import Criterion.Main (nf, bench, defaultMain, bgroup, defaultMainWith, defaultConfig)
import Criterion.Types (timeLimit)

import Math.NumberTheory.Prime (primesTo, primes)

-- Our benchmark harness.
main :: IO ()
main = defaultMainWith (defaultConfig {timeLimit = 10}) [
  bgroup "primesTo"
               [ bench "10"      $ nf primesTo 10
               , bench "100"     $ nf primesTo 100
               , bench "1000"    $ nf primesTo 1000
               , bench "100000"  $ nf primesTo 100000
               ]
  ]
