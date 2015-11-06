module Hilbert.Graph where
import qualified Data.Map as Map
import qualified Hilbert.PriorityQueue as PQ
import Data.Maybe (fromJust)

data Graph v e = Graph (Map.Map v (Map.Map v e))
  deriving Show

empty :: (Ord v, Ord e, Num e) => Graph v e
empty = Graph Map.empty

insert :: (Ord v, Ord e, Num e)
       => v -> v -> e -> Graph v e -> Graph v e
insert from to edge graph@(Graph vertexMap)
  = Graph $ newVertexMap
  where newVertexMap = Map.insert from newNeighborsMap vertexMap
        newNeighborsMap = Map.insert to edge neighborsMap
        neighborsMap = fromJust $ Map.lookup from vertexMap

delete :: (Ord v, Ord e, Num e)
       => v -> v -> Graph v e -> Graph v e
delete from to graph@(Graph vertexMap)
  = Graph $ newVertexMap
  where newVertexMap = Map.insert from newNeighborsMap vertexMap
        newNeighborsMap = Map.delete to neighborsMap
        neighborsMap = fromJust $ Map.lookup from vertexMap

changeEdge :: (Ord v, Ord e, Num e)
           => (e -> e) -> v -> v -> Graph v e -> Graph v e
changeEdge updateFunc from to graph@(Graph vertexMap)
  = Graph $ newVertexMap
  where newVertexMap = Map.insert from newNeighborsMap vertexMap
        newNeighborsMap = Map.adjust updateFunc to neighborsMap
        neighborsMap = fromJust $ Map.lookup from vertexMap

getEdge :: (Ord v, Ord e, Num e)
        => v -> v -> Graph v e -> Maybe e
getEdge from to graph@(Graph vertexMap)
  = case Map.lookup from vertexMap of
      Nothing -> Nothing
      Just neighborsMap -> Map.lookup to neighborsMap

fromList :: (Ord v, Ord e, Num e) => [(v, [(v, e)])] -> Graph v e
fromList graphList = 
  Graph $ Map.fromList
        $ map (\(a, b) -> (a, Map.fromList b))
          graphList

toList :: (Ord v, Ord e, Num e) => Graph v e -> [(v, [(v, e)])]
toList graph@(Graph vertexMap)
 = map (\(a, b) -> (a, Map.toList b)) $ Map.toList vertexMap

size :: Graph v e -> Int
size graph@(Graph vertexMap) = Map.size vertexMap

nodes :: (Ord v, Ord e, Num e) => Graph v e -> [v]
nodes graph = map fst $ toList graph

-- Dijkstra's algorithm
infinity :: (Ord a, Num a) => a
infinity = 123456789765432

dists graph = Map.fromList $ zip (nodes graph) $ 0:(repeat infinity)

visited graph = Map.fromList $ zip (nodes graph) $ repeat False

nodeQueue :: (Ord v, Ord e, Num e) =>
             Graph v e -> PQ.PriorityQueue v e 
nodeQueue graph = foldl foldFunc PQ.empty (Map.toList $ dists graph)
  where foldFunc queue (node, edge) = PQ.insert node edge queue

neighbors :: (Ord v, Ord e, Num e) => v -> Graph v e-> [v]
neighbors node graph@(Graph vertexMap) = 
  map fst $ Map.toList $ fromJust $ Map.lookup node vertexMap

dijkstra :: (Ord v, Ord e, Num e) => Graph v e -> v -> v -> e
dijkstra graph source sink = fromJust $ Map.lookup sink finalDists
  where (finalDists, _, _) = until collapseFunc (visitNode graph) 
                                    (dists, visited, nodeQueue)
        dists = Map.insert source 0
              $ Map.fromList $ zip (nodes graph) (repeat infinity)
        visited = Map.fromList $ zip (nodes graph) $ repeat False
        nodeQueue = foldl foldFunc PQ.empty (Map.toList dists)
          where foldFunc queue (node, edge) = PQ.insert node edge queue
        collapseFunc (_, _, q) = PQ.null q

visitNode :: (Ord v, Ord e, Num e) =>
             Graph v e -> (Map.Map v e,
                           Map.Map v Bool,
                           PQ.PriorityQueue v e)
                       -> (Map.Map v e,
                           Map.Map v Bool,
                           PQ.PriorityQueue v e)
visitNode graph (dists, visited, queue) =
      if fromJust $ Map.lookup closest visited
      then (dists, visited, rest)
      else (newDists, newVisited, newQueue)
  where newDists = foldl insertFunc dists newDistsToInsert
            where insertFunc dists (node, edge) = Map.insert node edge dists
        
        newDistsToInsert = zip neigh
                         $ zipWith min oldDists potentialNewDists

        newVisited = Map.insert closest True visited

        newQueue = foldl insertFunc rest newDistsToInsert
          where insertFunc rest (node, edge) = PQ.insert node edge rest
        (closest, rest) = PQ.deleteMin queue

        neigh = filter (\node -> not $ fromJust $ Map.lookup node visited)
                       (neighbors closest graph)

        oldDists = map (\node -> fromJust $ Map.lookup node dists) neigh

        potentialNewDists =
            map (\x -> cDist + (fromJust $ getEdge closest x graph)) neigh

        cDist = fromJust $ Map.lookup closest dists
