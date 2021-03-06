module Main (main) where

import Test.Hspec.Formatters (Formatter, specdoc)
import Test.Hspec.Runner     (configFormatter, defaultConfig, hspecWith)

import Spec                  (spec)

customFormatter :: Formatter
customFormatter = specdoc

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just customFormatter} Spec.spec
