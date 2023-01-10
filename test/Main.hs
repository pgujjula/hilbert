module Main (main) where

import Spec (spec)
import Test.Hspec.Formatters (Formatter, specdoc)
import Test.Hspec.Runner (configFormatter, defaultConfig, hspecWith)

customFormatter :: Formatter
customFormatter = specdoc

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just customFormatter} Spec.spec
