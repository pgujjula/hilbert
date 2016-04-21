module Hilbert.PriorityQueueTest (genMutations, testQueue, applyMutations,
                                  deleteAllMinWithPriority, deleteAllMin) where

import Hilbert.PriorityQueue as PQ
import Hilbert.List

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Applicative (liftA2)

import Data.List (foldl', sort)
import Data.Function (on)

{-
   Delete all minimum elements with the same priority.
-}
deleteAllMinWithPriority :: (PriorityQueueADT q, Ord p)
                         => (q v p) -> ([(v, p)], q v p)
deleteAllMinWithPriority queue = deleteWhilePriority minPty queue
  where deleteWhilePriority pty queue
          | PQ.null queue  = ([], queue)
          | currPty == pty = (first:rest, queue'')
          | otherwise      = ([], queue)
            where (first, queue') = deleteMinWithPriority queue
                  (rest, queue'') = deleteWhilePriority pty queue'
                  currPty         = snd first
        minPty = snd $ peekMinWithPriority queue

deleteAllMin :: (PriorityQueueADT q, Ord p) => q v p -> ([v], q v p)
deleteAllMin = clean . deleteAllMinWithPriority
  where clean (xs, q) = (map fst xs, q)

{-
   Represents the ways to mutate a single priority queue.
-}
data Mutation v p = Insert (v, p)
                | DeleteMinWithPriority 
                | DeleteMin
    deriving (Show)

{-
   Apply any mutation and return the new priority queue.
-}
applyMutation :: (Ord p, PriorityQueueADT q) => 
                    Mutation v p -> q v p -> q v p
applyMutation (Insert (v, p))         queue =
    insert v p queue
applyMutation (DeleteMinWithPriority) queue
  | PQ.null queue = queue
  | otherwise     =  snd $ deleteAllMinWithPriority queue
applyMutation DeleteMin queue
  | PQ.null queue = queue
  | otherwise     = snd $ deleteAllMin queue

{-
   Apply a list of mutations and return the new priority queue.
-}
applyMutations :: (Ord p, PriorityQueueADT q) =>
                    [Mutation v p] -> q v p -> q v p
applyMutations mutationList queue = 
  foldl' (flip applyMutation) queue mutationList

{-
   Represents the ways to query a single priority queue.
-}
data Query v p = Null 
               | Size
               | PeekMin
               | PeekMinWithPriority

               -- Very ugly naming, hopefully haskell 
               -- record library can fix this.
               | DeleteMin'
               | DeleteMinWithPriority'

data QueryResult v p = NullResult Bool
                     | SizeResult Int
                     | ValueResult v
                     | ValuePriorityResult (v, p)
    deriving (Eq)

applyQuery :: (Ord p, PriorityQueueADT q) =>
                Query v p -> q v p -> QueryResult v p
applyQuery Null queue = NullResult (PQ.null queue)        
applyQuery Size queue = SizeResult (size queue)
applyQuery PeekMin queue = ValueResult (peekMin queue)
applyQuery PeekMinWithPriority queue = 
  ValuePriorityResult (peekMinWithPriority queue)
applyQuery DeleteMin' queue =
  ValueResult (fst $ deleteMin queue)
applyQueue DeleteMinWithPriority' queue =
  ValuePriorityResult (fst $ deleteMinWithPriority queue)

{-
   Generate a random mutation given generators for the
   value and priority data types.
-}
genMutation :: Int -> Gen v -> Gen p -> Gen (Mutation v p)
genMutation insertRatio valueGen ptyGen =
  frequency
    [ (2*insertRatio, insertGen)
    , (1, return DeleteMinWithPriority)
    , (1, return DeleteMin)
    ]
  where insertGen = do
            v <- valueGen
            p <- ptyGen
            return (Insert (v, p))

{-
   Generate a random list of mutations of the given length
   using the given generators for value and priority
-}
genMutations :: Int -> Int -> Gen v -> Gen p -> Gen ([Mutation v p])
genMutations size insertRatio valueGen ptyGen =
  vectorOf size $ genMutation insertRatio valueGen ptyGen

{- Generators for random Ints and Strings -}
intGen :: Gen Int
intGen = choose (1, 10)

{- 
   randomQueueGen mutationGen n generates a random queue
   by applying n random mutations obtained from mutationGen.
-}
randomQueueGen :: (Ord p, PriorityQueueADT q)
               => Gen (Mutation v p) -> Int
               -> Gen (q v p)
randomQueueGen = randomQueueGenWith empty
  where randomQueueGenWith queue mutationGen numMutations
          | numMutations == 0   = return queue
          | otherwise = do
              mutation <- mutationGen
              let queue' = applyMutation mutation queue
              randomQueueGenWith queue' mutationGen (numMutations - 1)

{- The ratio of inserts to deletes. -}
insertRatio :: Int
insertRatio = 200000000

{- The number of mutations to perform. -}
numMutations :: Int
numMutations = 100

{- Convert a queue to a list by removing the elements in order -}
toList :: (PriorityQueueADT q, Ord p) => q v p -> [(v, p)]
toList queue
  | PQ.null queue = []
  | otherwise  = first:rest
      where (first, queue') = deleteMinWithPriority queue
            rest = toList queue'

testQueue :: (PriorityQueueADT control, PriorityQueueADT test, Ord p, Ord v, Show p, Show v)
          => control v p -> test v p -> [Mutation v p] -> Expectation
testQueue control test mutations = list2 `shouldBe` list1
  where list1 = collapse control 
        list2 = collapse test
        collapse queue = map sort
                       $ groupBy (compare `on` snd)
                       $ toList
                       $ applyMutations mutations queue
