-- | The interface to arbitrary graphs.
module Hilbert.Graph.ADT where

{- | A type for indicating whether a graph is 
     directed or undirected.
-}
data GraphType = Undirected
               | Directed
      deriving (Eq, Show)

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
  null :: g v e -> Bool

  -- | Get the type of a graph (Undirected/Directed)
  graphType :: g v e -> GraphType

  -- | Returns the number of vertices in the graph.
  numVertices :: (Ord v) => g v e -> Int

  -- | Returns the number of edges in the graph.
  numEdges :: g v e -> Int

  {- | Inserts a vertex with no neighbors into the graph.
       Does nothing if the vertex already exists.
  -}
  insertVertex :: (Ord v) => v -> g v e -> g v e

  {- | Inserts an edge between two vertices. An existing edge
       between the two vertices will be replaced. If one or both
       of the vertices does not exist, then they will be added.
  -}
  insertEdge :: (Ord v, Ord e) => v -> v -> e -> g v e -> g v e

  {- | Deletes a vertex from the graph. Does nothing if the
       vertex does not exist. Deletes all associated edges as well.
  -}
  deleteVertex :: (Ord v) => v -> g v e -> g v e

  {- | Deletes an edge from the graph. Does nothing if the 
       edge does not exist.
  -}
  deleteEdge :: (Ord v, Num v) => v -> v -> g v e -> g v e

  {- | Finds the edge weight between two connected vertices.
       Returns Nothing if the vertices are not connected.
  -}
  weight :: (Ord v) => v -> v -> g v e -> Maybe e

  -- | Returns whether the two vertices are connected.
  isNeighbor :: (Ord v) => v -> v -> g v e -> Bool

  -- | Returns a list of neighbors of the vertex.
  neighbors :: (Ord v) => v -> g v e -> [v]

  -- | Convert a list to a graph.
  fromList :: (Ord v, Ord e) => GraphType -> [(v, v, e)] -> g v e

  -- | Get the vertices of a graph
  vertices :: (Ord v) => g v e -> [v]

  -- | Get the edges of a graph
  edges :: (Ord v) => g v e -> [(v, v, e)]
