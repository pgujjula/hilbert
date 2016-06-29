module Hilbert.PriorityQueue.MapSpec
  ( main
  , spec
  ) where

import Hilbert.PriorityQueue.ADT
import Hilbert.PriorityQueue.Map
import Hilbert.PriorityQueueTest
import Hilbert.PriorityQueue.List
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
           test 1   5
           test 5   5
           test 100 5
           test 1   100
           test 5   100
           test 100 100

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
intGen = choose (1, 10)

emptyListQueue = empty :: ListQueue String Int
emptyMapQueue   = empty :: MapQueue String Int

{-
   MapQueue test: Compare MapQueue and ListQueue using the given insert
   ratio and number of mutations.
-}
test insertRatio numMutations = 
   it ("Comparing MapQueue and ListQueue with insertRatio = "
          ++ (show insertRatio)
          ++ " and numMutations = "
          ++ (show numMutations)
      ) $ do
      forAll mutationGen  $ \mutations ->
        testQueue emptyListQueue emptyMapQueue mutations
  where mutationGen = genMutations numMutations insertRatio stringGen intGen
