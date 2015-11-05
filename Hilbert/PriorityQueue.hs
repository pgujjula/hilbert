module Hilbert.PriorityQueue (insert, deleteMin, empty,
                      peekMin, PriorityQueue, Hilbert.PriorityQueue.null) where
import qualified Data.Map as Map

data PriorityQueue e p = PriorityQueue (Map.Map p [e])
  deriving (Show)

null :: PriorityQueue e p -> Bool
null queue@(PriorityQueue m) = Map.null m

-- Inserts "element" into "queue" with the given "priority"
insert :: (Ord p) => e -> p -> PriorityQueue e p -> PriorityQueue e p
insert element priority queue@(PriorityQueue m)
 = PriorityQueue 
  $ Map.insertWith (++) priority [element] m

deleteMin :: (Ord p) => PriorityQueue e p -> (e, PriorityQueue e p)
deleteMin queue@(PriorityQueue m) = (minElem, newQueue)
  where minNode = Map.findMin m
        (minPriority, minElem:otherElems) = minNode
        newQueue = PriorityQueue 
                 $ if Prelude.null otherElems
                   then Map.delete minPriority m
                   else Map.insert minPriority otherElems m

empty :: (Ord p) => PriorityQueue e p
empty = PriorityQueue $ Map.empty

peekMin :: (Ord p) => PriorityQueue e p -> e
peekMin queue@(PriorityQueue m)
  = head $ snd $ Map.findMin m
