module Hilbert.PriorityQueue.MapSpec (main, spec) where

import Hilbert.PriorityQueue.ADT
import Hilbert.PriorityQueue.Map
import Hilbert.PriorityQueueTest
import Hilbert.PriorityQueue.Naive
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec spec

numberOfTests :: Int
numberOfTests = 500

numMutations :: Int
numMutations = 200

insertRatio :: Int
insertRatio = 20

stringGen :: Gen String
stringGen = resize 3
          $ listOf
          $ choose ('a', 'z')

intGen :: Gen Int
intGen = choose (1, 100)

emptyNaiveQueue = empty :: NaiveQueue String Int
emptyMapQueue   = empty :: MapQueue String Int

mutationGen = genMutations numMutations insertRatio stringGen intGen

spec = modifyMaxSuccess (\_ -> numberOfTests) $ 
           it "Random mutations have the same effect on MapQueue as on NaiveQueue" $ do
             forAll mutationGen $ \mutations ->
               testQueue emptyNaiveQueue emptyMapQueue mutations
