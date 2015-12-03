module Hilbert.Graph.ADT where

{- |The interface for arbitrary graphs. Use Hilbert.Graph.Undirected
    or Hilbert.Graph.Directed based on your needs. -}
class GraphADT g where
  -- | The empty graph.
  empty :: g e v

  -- | Returns true if a graph is empty.
  null :: g e v -> Bool

  -- | Returns the number of vertices in the graph.
  numVertices :: g e v -> Int

  -- | Returns the number of edges in the graph.
  numEdges :: g e v -> Int

  -- | Inserts a vertex with no neighbors into the graph.
  --   Does nothing if the vertex already exists.
  insertVertex :: (Ord v) => v -> g e v -> g e v

  -- | Inserts an edge between two vertices. An existing edge
  --   between the two vertices will be replaced.
  insertEdge :: (Ord e) => v -> v -> e -> g e v -> g e v

  -- | Deletes a vertex from the graph. Does nothing if the
  --   vertex does not exist. Deletes all associated edges as well.
  deleteVertex :: (Ord v) => v -> g e v -> g e v

  -- | Deletes an edge from the graph. Does nothing if the 
  --   edge does not exist.
  deleteEdge :: (Ord v, Num v) => v -> v -> g e v -> g e v

  -- | Finds the edge weight between two connected vertices
  weight :: (Ord v) => v -> v -> g e v -> Maybe e

  -- | Returns whether the two vertices are connected.
  isNeighbor :: (Ord v) => v -> v -> g e v -> Bool

  -- | Returns a list of neighbors of the vertex.
  neighbors :: (Ord v) => v -> g e v -> [v]
