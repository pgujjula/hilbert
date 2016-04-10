module Hilbert.PriorityQueueSpec () where

import Hilbert.PriorityQueue as PQ
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Applicative (liftA2)

{-
  Represents the ways to mutate a single priority queue
-}
data Mutation v p = Insert (v, p)
                | DeleteMinWithPriority 
                | DeleteMin
    deriving (Show)

applyMutation :: (Ord p, PriorityQueueADT q) => 
                    Mutation v p -> q v p -> q v p
applyMutation (Insert (v, p))         queue =
    insert v p queue
applyMutation (DeleteMinWithPriority) queue
  | PQ.null queue = queue
  | otherwise     =  snd $ deleteMinWithPriority queue
applyMutation DeleteMin queue
  | PQ.null queue = queue
  | otherwise     = snd $ deleteMin queue

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
  value and priority data types
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

{- The ratio of inserts to deletes -}
insertRatio :: Int
insertRatio = 2

intGen :: Gen Int
intGen = choose (1, 10)

stringGen :: Gen String
stringGen = elements 
              ["CALIFORNIA",
               "OHIO",
               "ARKANSAS", 
               "RHODE ISLAND",
               "NEW YORK"]

{- A generator for mutations -}
mutationGen :: Gen (Mutation Int String)
mutationGen = genMutation insertRatio intGen stringGen

randomQueueGen :: (Ord p)
               => Gen (Mutation v p) -> Int
               -> Gen (MapQueue v p)
randomQueueGen = randomQueueGenWith empty

randomQueueGenWith :: (Ord p) 
                   => MapQueue v p 
                   -> Gen (Mutation v p)
                   -> Int
                   -> Gen (MapQueue v p)
randomQueueGenWith queue mutationGen numMutations
  | numMutations == 0   = return queue
  | otherwise = do
      mutation <- mutationGen
      let queue' = applyMutation mutation queue
      randomQueueGenWith queue' mutationGen (numMutations - 1)

randomQueue :: Gen (MapQueue Int String)
randomQueue = randomQueueGen mutationGen 10

compareQueue :: (PriorityQueueADT a, PriorityQueueADT b, Ord p) 
             => a v p -> b v p-> Gen (Mutation v p)
             -> (a v p, b v p)
compareQueue q1 q2 mutationGen = 
