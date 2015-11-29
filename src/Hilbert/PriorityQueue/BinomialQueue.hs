{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
module Hilbert.PriorityQueue.BinomialQueue
  (BinomialQueue) where

import Data.Maybe (catMaybes)
import Data.List (minimumBy, intersperse)
import Control.Applicative ((<$>), liftA2)
import Hilbert.PriorityQueue.BinomialQueue.BinomialTree
  (BinomialTree, rank, root, debugPrintTree, link, singleton, children)

import qualified Hilbert.PriorityQueue.BinomialQueue.BinomialTree as Tree (fromList)
import Hilbert.PriorityQueue.ADT

newtype BinomialQueue v p = BinomialQueue
                              { getList :: [Maybe (BinomialTree v p)]}
  deriving (Show)

insertTree :: (Ord p) => Maybe (BinomialTree v p)
                      -> [Maybe (BinomialTree v p)]
                      -> [Maybe (BinomialTree v p)]
insertTree mtree (Nothing:xs) = mtree:xs
insertTree Nothing xs = xs
insertTree (Just tree) ((Just x):xs) = Nothing:(insertTree (Just (link tree x)) xs)
insertTree mtree [] = [mtree]


debugPrintQueue :: (Show v, Show p) => BinomialQueue v p -> IO ()
debugPrintQueue = sequence_
                . (intersperse (putStr "\n"))
                . (map printFunc)
                . getList
  where printFunc (Just x) = debugPrintTree x
        printFunc Nothing  = putStrLn "Nil"

instance PriorityQueueADT BinomialQueue where
  empty :: BinomialQueue v p
  empty = BinomialQueue []

  null :: BinomialQueue v p -> Bool
  null = Prelude.null . getList

  size :: BinomialQueue v p -> Int
  size = sum . (map rank) . catMaybes . getList

  insert :: (Ord p) => v -> p -> BinomialQueue v p -> BinomialQueue v p
  insert val pty (BinomialQueue list) =
        BinomialQueue $ insertTree (Just $ singleton val pty) list


  peekMinWithPriority :: (Ord p) => BinomialQueue v p -> (v, p)
  peekMinWithPriority = (minimumBy (\(v1, p1) (v2, p2) -> compare p1 p2))
                      . (map root)
                      . catMaybes
                      . getList

  deleteMinWithPriority :: forall v p . (Ord p)
                        => BinomialQueue v p
                        -> ((v, p), BinomialQueue v p)
  deleteMinWithPriority (BinomialQueue list) = (minElem, newQueue)
    where (minIndex, minTree) = findMinTree list

          listMinusTree = (take minIndex list)
                       ++ (Nothing:(drop (minIndex + 1) list))

          treeMinusRoot :: [BinomialTree v p]
          treeMinusRoot = reverse $ children minTree

          minElem :: (v, p)
          minElem = root minTree

          newQueue :: BinomialQueue v p
          newQueue = meld (BinomialQueue listMinusTree)
                          (BinomialQueue $ map Just treeMinusRoot)

          findMinTree :: [Maybe (BinomialTree v p)] -> (Int, BinomialTree v p)
          findMinTree = findMinTree' 0 
          
          findMinTree' :: Int -> [Maybe (BinomialTree v p)] -> (Int, BinomialTree v p)
          findMinTree' n [Just tree] = (n, tree) 
          findMinTree' n (Nothing:mtree:rest) = findMinTree' (n + 1) (mtree:rest)
          findMinTree' n (mtree:Nothing:rest) = findMinTree' n (mtree:rest)
          findMinTree' n ((Just t1):(Just t2):rest) =
            if (snd $ root t1) < (snd $ root t2)
            then findMinTree' n ((Just t1):rest)
            else findMinTree' (n + 1) ((Just t2):rest)

  meld :: (Ord p) => BinomialQueue v p
                              -> BinomialQueue v p
                              -> BinomialQueue v p
  meld (BinomialQueue xs) (BinomialQueue ys) =
      BinomialQueue $ meld' Nothing xs ys
    where meld' carry (x:xs) (y:ys) = z:(meld' newCarry xs ys)
            where (newCarry, z) = add3 [carry, x, y]
          meld' carry [] ys = insertTree carry ys
          meld' carry xs [] = insertTree carry xs

          -- Returns (carry, result)
          add3 :: (Ord p) => [Maybe (BinomialTree v p)]
                         -> (Maybe (BinomialTree v p),
                             Maybe (BinomialTree v p))
          add3 list = case length nonzeros of 
                       0 -> (Nothing, Nothing)
                       1 -> (Nothing, Just (nonzeros !! 0))
                       2 -> (Just $ link (nonzeros !! 0) (nonzeros !! 1), Nothing)
                       3 -> (Just $ link (nonzeros !! 0) (nonzeros !! 1), 
                               Just $ nonzeros !!  2)
                    where nonzeros = catMaybes list

  fromList :: (Ord p) => [(v, p)] -> BinomialQueue v p
  fromList list = BinomialQueue queueList
    where len = length list
          binList = binaryListWithPowers len
          groups = group list binList
          queueList = map convert groups

          convert :: (Ord p) => Maybe [(v, p)] -> Maybe (BinomialTree v p)
          convert = (<$>) (Tree.fromList)

          group :: (Ord p) => [(v, p)] -> [(Int, Int)] -> [Maybe [(v, p)]]
          group [] [] = []
          group list ((0, _):restBinary) = Nothing:(group list restBinary)
          group list ((1, power):restBinary) = (Just first):(group rest restBinary)
                where (first, rest) = splitAt power list

          binaryListWithPowers :: Int -> [(Int, Int)]
          binaryListWithPowers n = zip (binaryList n) powersOfTwo
            where binaryList :: Int -> [Int]
                  binaryList 0 = []
                  binaryList n = (fromIntegral $ n `rem` 2) : (binaryList $ n `div` 2)

          powersOfTwo :: [Int]
          powersOfTwo = scanl (*) 1 (repeat 2)
