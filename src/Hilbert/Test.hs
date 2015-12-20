module Hilbert.Test where

import System.Exit (exitFailure, exitSuccess)
import Data.Maybe (isJust, fromJust)

data Test = Single
            {
              expression :: String,
              expected   :: String,
              actual     :: String
            }
          | Chain [Test]

runtest :: Test -> Maybe String
runtest test@(Single _ _ _) =
  if (expected test) == (actual test)
  then Nothing
  else Just $ "ERROR! In computing: " ++ (expression test) ++ "\n"
    ++ "  expected: " ++ (expected test) ++ "\n"
    ++ "  but got : " ++ (actual test) ++ "\n"

runtest test@(Chain ts) = if null errors
                          then Nothing
                          else Just (concat errors)
      where errors = map fromJust $ filter isJust $ map runtest ts

mruntest :: Test -> IO ()
mruntest test = case runtest test of
                  Nothing -> exitSuccess
                  Just err -> do
                                putStrLn err
                                exitFailure
