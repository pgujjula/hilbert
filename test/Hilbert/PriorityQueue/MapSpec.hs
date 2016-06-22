module Hilbert.PriorityQueue.MapSpec
  ( main
  , spec
  ) where

import Hilbert.PriorityQueue.ADT
import Hilbert.PriorityQueue.Map
import Hilbert.PriorityQueueTest
import Hilbert.PriorityQueue.Naive
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec = modifyMaxSuccess (\_ -> numberOfTests) $ 
         describe "MapQueue" $ do
           test 1   2
           test 5   2
           test 100 2
           test 1   200
           test 5   200
           test 100 200

{-
   Supplementary data
-}
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

{-
   MapQueue test: Compare MapQueue and NaiveQueue using the given insert
   ratio and number of mutations.
-}
test insertRatio numMutations = 
   it ("Comparing MapQueue and NaiveQueue with insertRatio = "
          ++ (show insertRatio)
          ++ " and numMutations = "
          ++ (show numMutations)
      ) $ do
      forAll mutationGen  $ \mutations ->
        testQueue emptyNaiveQueue emptyMapQueue mutations
  where mutationGen = genMutations numMutations insertRatio stringGen intGen
