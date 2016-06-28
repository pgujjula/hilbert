-- This module lays out a testing framework for all priority queue.
module Hilbert.PriorityQueueTest
  ( genMutations
  , testQueue
  , applyMutations
  ) where

import Hilbert.PriorityQueue as PQ
import Hilbert.List

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Applicative (liftA2)

import Data.List (foldl', sort)
import Data.Function (on)

{-
   Delete all minimum elements with the same priority. Returns the deleted
   (value, priority) pairs and the new queue.
-}
deleteWhileP :: (PriorityQueueADT q, Ord p)
                => (p -> Bool) -> q v p -> ([(v, p)], q v p)
deleteWhileP func queue
  | PQ.null queue = ([], queue)
  | func minP = (vp:vps, restQueue)
  | otherwise = ([], queue)
    where (_, minP) = peekMinP queue
          (vp, queue') = deleteMinP queue
          (vps, restQueue) = deleteWhileP func queue'

{-
   Delete all minimum elements with the same priority. Returns the deleted
   values and the new queue.
-}
deleteWhile :: (PriorityQueueADT q, Ord p)
                => (p -> Bool) -> q v p -> ([v], q v p)
deleteWhile func queue
  | PQ.null queue = ([], queue)
  | func minP = (v:vs, restQueue)
  | otherwise = ([], queue)
      where (v, minP) = peekMinP queue
            (_, queue') = deleteMin queue
            (vs, restQueue) = deleteWhile func queue'

{-
   Represents the ways to mutate a single priority queue.
-}
data Mutation v p = Insert (v, p)
                | DeleteMin
                | DeleteMinP
                | DeleteAllMin
                | DeleteAllMinP
    deriving (Show)

{-
   Apply any mutation and return the new priority queue.
-}
applyMutation :: (Ord p, PriorityQueueADT q) => 
                    q v p -> Mutation v p -> q v p

applyMutation queue (Insert (v, p)) = insert v p queue
applyMutation queue mutation | PQ.null queue = queue
applyMutation queue mutation = 
  case mutation of
    Insert (v, p) -> insert v p queue
    DeleteMin     -> snd $ deleteWhile (== minP) queue
    DeleteMinP    -> snd $ deleteWhileP (== minP) queue
    DeleteAllMin  -> snd $ deleteAllMin queue
    DeleteAllMinP -> snd $ deleteAllMinP queue
  where minP = snd $ peekMinP queue

{-
   Apply a list of mutations and return the new priority queue.
-}
applyMutations :: (Ord p, PriorityQueueADT q)
               => q v p -> [Mutation v p] -> q v p
applyMutations = foldl' applyMutation 

{-
   Generate a random mutation given generators for the
   value and priority data types.
-}
genMutation :: Int -> Gen v -> Gen p -> Gen (Mutation v p)
genMutation insertRatio valueGen ptyGen =
  frequency
    [ (4*insertRatio, insertGen)
    , (1, return DeleteMin)
    , (1, return DeleteMinP)
    , (1, return DeleteAllMin)
    , (1, return DeleteAllMinP)
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

{- Convert a queue to a list by removing the elements in order -}
toList :: (PriorityQueueADT q, Ord p) => q v p -> [(v, p)]
toList queue
  | PQ.null queue = []
  | otherwise  = first:rest
      where (first, queue') = deleteMinP queue
            rest = toList queue'

-- Compare the queries on the two queues
compareQueues :: (PriorityQueueADT control, PriorityQueueADT test,
                  Ord p, Ord v, Show p, Show v)
              => control v p -> test v p -> Expectation
compareQueues control test = do
  (PQ.null control) `shouldBe` (PQ.null test)
  (size control) `shouldBe` (size test)
  if not (PQ.null control)
  then do 
    (fst $ deleteAllMin test) `shouldContain` [peekMin control]
    (fst $ deleteAllMin control) `shouldContain` [peekMin test]
    (mold $ fst $ deleteAllMinP test) `shouldContain` [peekMinP control]
    (mold $ fst $ deleteAllMinP control) `shouldContain` [peekMinP test]
    (fst $ deleteMin test) `shouldBe` (fst $ deleteMin control)
    (fst $ deleteMinP test) `shouldBe` (fst $ deleteMinP control)
    (fst $ deleteAllMin test) `shouldBe` (fst $ deleteAllMin control)
    (fst $ deleteAllMinP test) `shouldBe` (fst $ deleteAllMinP control)
  else return ()
    where mold (xs, y) = zip xs (repeat y)

{-
   Test the "test" priority queue type against the "control" type, using the given
   list of mutations
-}
testQueue :: (PriorityQueueADT control, PriorityQueueADT test, Ord p, Ord v, Show p, Show v)
          => control v p -> test v p -> [Mutation v p] -> Expectation
testQueue control test mutations = do 
    list2 `shouldBe` list1
--    compareQueues controlQueue testQueue
  where list1 = collapse control 
        list2 = collapse test
        controlQueue = applyMutations control mutations
        testQueue = applyMutations test mutations
        collapse queue = -- Sort each group of values
                         map sort
                         -- Group values by priority
                         $ groupBy (compare `on` snd)
                         -- A sorted list of (value, priority) pairs, sorted 
                         -- by priority
                         $ toList
                         $ applyMutations queue mutations
