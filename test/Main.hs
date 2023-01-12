module Main (main) where

import Spec (spec)
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  ts <- testSpec "" Spec.spec
  defaultMain ts
