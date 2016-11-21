module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import System.IO (hPutStr, hFlush)
import Data.List (intersperse)

customFormatter :: Formatter
customFormatter = specdoc 

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just customFormatter} Spec.spec
