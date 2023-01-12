module Main (main) where

import Spec (spec)
import Test.Hspec.Formatters (Formatter, specdoc)
import Test.Hspec.Runner (configFormatter, defaultConfig, hspecWith)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  ts <- testSpec "" Spec.spec
  defaultMain ts
