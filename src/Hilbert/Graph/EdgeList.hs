{- | An implementation of GraphADT using a simple
     edge list. This is probably not efficient for 
     most applications, but it is good for debugging
     better structures. 
-}
module Hilbert.Graph.EdgeList where

import Data.List (sort)

import Hilbert.Graph.ADT
import Hilbert.List (rmDups)

data EdgeList v e = EdgeList {
                      gtype :: GraphType, -- ^so as not to conflict with ADT.graphType
                      vs  :: [v],
                      es :: [(v, v, e)]
                    } 
        deriving (Show, Eq)

swap :: (v, v, e) -> (v, v, e)
swap (v1, v2, e) = (v2, v1, e)

compareSwap :: (Ord v) => (v, v, e) -> (v, v, e)
compareSwap (v1, v2, e) = if v2 < v1
                          then (v2, v1, e)
                          else (v1, v2, e)

cmp (v1, _, _) (v2, _, _) = compare v1 v2
sameVertex (v1, _, _) (v2, _, _) = v1 == v2

instance GraphADT EdgeList where
  emptyUndirected = EdgeList Undirected [] []
  emptyDirected   = EdgeList Directed [] []

  null = Prelude.null . vs

  graphType = gtype

  numVertices = length . vs

  numEdges = length . es

  insertVertex v graph@(EdgeList gtype vs es) =
      if v `elem` vs
      then graph
      else EdgeList gtype (v:vs) es

  insertEdge v1 v2 e graph@(EdgeList Directed _ _) = graphWithEdge
    where graphWithVertices = insertVertex v1 (insertVertex v2 graph)
          graphWithEdge = if (v1, v2, e) `elem` (es graphWithVertices)
                          then graphWithVertices
                          else EdgeList Directed (vs graphWithVertices)
                                            ((v1, v2, e):(es graphWithVertices))

  insertEdge v1 v2 e graph@(EdgeList Undirected _ _) = graphWithEdge
    where graphWithVertices = insertVertex v1 (insertVertex v2 graph)
          graphWithEdge = if (v1, v2, e) `elem` (es graphWithVertices)
                           || (v2, v1, e) `elem` (es graphWithVertices)
                          then graphWithVertices
                          else EdgeList Undirected (vs graphWithVertices)
                                          ((v1, v2, e):(es graphWithVertices))

  deleteVertex = undefined
  deleteEdge = undefined
  weight = undefined
  isNeighbor = undefined
  neighbors vertex graph = undefined

  fromList = undefined
  vertices = vs
  edges = es
