module Main where

import System.Exit (exitFailure, exitSuccess)
import Hilbert.Graph.ADT as ADT
import Hilbert.Graph.EdgeList
import Data.Maybe

{- Test that emptyUndirected returns an empty graph. -}
test1 :: Maybe String
test1 = if actual == expected
        then Nothing
        else Just $ "In computing: null emptyUndirected\n"
          ++ "  expected: " ++ (show expected) ++ "\n"
          ++ "  but got: " ++ (show actual) ++ "\n"
    where actual = ADT.null (emptyUndirected::EdgeList Int Int)
          expected = True

{- Test that emptyUndirected returns an Undirected graph. -}
test2 :: Maybe String
test2 =  if actual == expected
         then Nothing
         else Just $ "In computing: graphType emptyUndirected\n"
          ++ "expected: " ++ (show expected) ++ "\n"
          ++ "but got : " ++ (show actual)   ++ "\n"
    where actual = graphType (emptyUndirected::EdgeList Int Int)
          expected = Undirected

{- Chain tests together into a new test. -}
chain :: [Maybe String] -> Maybe String
chain tests = if Prelude.null errors
        then Nothing
        else Just (concat errors)
    where errors = map fromJust $ filter isJust tests

{- Run a test. -}
runTest Nothing = exitSuccess
runTest (Just err) = do
                  putStrLn err
                  exitFailure

main = runTest $ chain [test1, test2]
