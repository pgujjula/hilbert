module Main where

import System.Exit (exitFailure, exitSuccess)
import Hilbert.Graph.ADT as ADT
import Hilbert.Graph.EdgeList
import Hilbert.Test
import Data.Maybe

{- Test that emptyUndirected returns an empty graph. -}
test1 = Single "null emptyUndirected::(EdgeList Int Int)"
               (show True)
               (show $ ADT.null (emptyUndirected::(EdgeList Int Int)))

{- Test that emptyUndirected returns an Undirected graph. -}
test2 = Single "graphType emptyUndirected::(EdgeList Int Int)"
               (show Undirected)
               (show $ graphType (emptyUndirected::(EdgeList Int Int)))

main = mruntest (Chain [test1, test2])
