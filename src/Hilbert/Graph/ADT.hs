-- | The interface to arbitrary graphs.
module Hilbert.Graph.ADT where

{- | A type for indicating whether a graph is 
     directed or undirected.
-}
data GraphType = Undirected
               | Directed

{- | The graph typeclass. Implementations are provided in 
     other modules. A good default implementation is provided in
     Hilbert.Graph.
-}
class GraphADT g where
  -- | An empty undirected graph.
  emptyUndirected :: g v e

  -- | An empty directed graph.
  emptyDirected :: g v e 

  -- | Returns true if a graph is empty.
  null :: g e v -> Bool

  -- | Returns the number of vertices in the graph.
  numVertices :: g e v -> Int

  -- | Returns the number of edges in the graph.
  numEdges :: g e v -> Int

  {- | Inserts a vertex with no neighbors into the graph.
       Does nothing if the vertex already exists.
  -}
  insertVertex :: (Ord v) => v -> g e v -> g e v

  {- | Inserts an edge between two vertices. An existing edge
       between the two vertices will be replaced.
  -}
  insertEdge :: (Ord e) => v -> v -> e -> g e v -> g e v

  {- | Deletes a vertex from the graph. Does nothing if the
       vertex does not exist. Deletes all associated edges as well.
  -}
  deleteVertex :: (Ord v) => v -> g e v -> g e v

  {- | Deletes an edge from the graph. Does nothing if the 
       edge does not exist.
  -}
  deleteEdge :: (Ord v, Num v) => v -> v -> g e v -> g e v

  -- | Finds the edge weight between two connected vertices.
  weight :: (Ord v) => v -> v -> g e v -> Maybe e

  -- | Returns whether the two vertices are connected.
  isNeighbor :: (Ord v) => v -> v -> g e v -> Bool

  -- | Returns a list of neighbors of the vertex.
  neighbors :: (Ord v) => v -> g e v -> [v]

  -- | Convert a list to a graph.
  fromList :: (Ord v, Ord e) => GraphType -> [(v, v, e)] -> g v e

  -- | Convert a graph to a list.
  toList :: g v e -> (GraphType, [(v, v, e)])
