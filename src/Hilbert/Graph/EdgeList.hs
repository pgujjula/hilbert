{- | An implementation of GraphADT using a simple
     edge list. This is probably not efficient for 
     most applications, but it is good for debugging
     better structures. 
-}
module Hilbert.Graph.EdgeList where

import Hilbert.Graph.ADT

data EdgeList v e = EdgeList {
                      graphType :: GraphType,
                      list      :: [(v, v, e)]
                    } 

instance GraphADT EdgeList where
  emptyUndirected = EdgeList Undirected []
  emptyDirected =   EdgeList Directed []

  null = Prelude.null . list

  numVertices = undefined

  numEdges = length . list

  insertVertex = undefined
  insertEdge = undefined
  deleteVertex = undefined
  deleteEdge = undefined
  weight = undefined
  isNeighbor = undefined
  neighbors vertex graph = undefined

  fromList = EdgeList
  toList g = (graphType g, list g)
