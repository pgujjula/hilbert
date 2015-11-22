module Hilbert.BinomialQueue
        (empty
        , fromList
        , peekMin
        , deleteMin
        , insert
        , meld
        , printQueue
        , BinomialQueue)
  where

import Data.Maybe (isJust, fromJust)
import Data.List (null)

data BinomialTree a = BinomialTree a [BinomialTree a]
  deriving (Show, Ord, Eq)

children :: BinomialTree a -> [BinomialTree a]
children (BinomialTree r c) = c

root :: BinomialTree a -> a
root (BinomialTree r c) = r

singletonTree :: (Ord a) => a -> BinomialTree a
singletonTree x = BinomialTree x []

-- Merges two binomial trees
-- Preconditions: both trees must have the same order
--   and must be heap-balanced (preconditions not checked.)
mergeTree :: (Ord a) => BinomialTree a -> BinomialTree a -> BinomialTree a
mergeTree t1@(BinomialTree r1 l1)
          t2@(BinomialTree r2 l2) = 
      if r1 < r2
      then BinomialTree r1 (t2:l1) 
      else BinomialTree r2 (t1:l2)

type BinomialQueue a = [Maybe (BinomialTree a)]

empty :: BinomialQueue a
empty = []

insertTree :: (Ord a) => BinomialTree a -> BinomialQueue a -> BinomialQueue a
insertTree s (t:ts) = 
  case t of
    Nothing -> ((Just s):ts)
    Just a  -> Nothing:(insertTree (mergeTree s a) ts)
insertTree s [] = [Just s]

insert :: (Ord a) => a -> BinomialQueue a -> BinomialQueue a
insert = insertTree . singletonTree

fromList :: (Ord a) => [a] -> BinomialQueue a
fromList (x:xs) = insert x $ fromList xs
fromList _ = empty

addTree3 :: (Ord a) => Maybe (BinomialTree a)
                    -> Maybe (BinomialTree a)
                    -> Maybe (BinomialTree a) 
                    -> (Maybe (BinomialTree a), Maybe (BinomialTree a))
addTree3 t1 t2 t3 = 
  case nonTrivial of
    [] -> (Nothing, Nothing)
    [x] -> (Just x, Nothing)
    [x1, x2] -> (Nothing, Just (mergeTree x1 x2))
    [x1, x2, x3] -> (Just x3, Just (mergeTree x1 x2))
  where nonTrivial = map fromJust $ filter isJust [t1, t2, t3]

meld :: (Ord a) => BinomialQueue a -> BinomialQueue a -> BinomialQueue a
meld = meld' Nothing
  where meld' carry (x:xs) (y:ys) = first:rest
          where (first, carry2) = addTree3 carry x y
                rest = meld' carry2 xs ys
        meld' carry xs ys =
          case carry of
            Nothing -> nonempty
            Just y  -> insertTree y nonempty
          where nonempty = if null xs
                           then ys
                           else xs

peekMin :: (Ord a) => BinomialQueue a -> a
peekMin = minimum . (map (root . fromJust)) . (filter isJust) 

minIndexWithElem' :: (Ord a) => (a -> a -> Ordering) -> (Int, a) -> Int -> [a] -> (Int, a)
minIndexWithElem' func (n, y) curr (x:xs) =
           if (func x y) == LT
           then minIndexWithElem' func (curr, x) (curr + 1) xs
           else minIndexWithElem' func (n, y) (curr + 1) xs
minIndexWithElem' _ (n, y) _ _ = (n, y)

minIndexWithElem func xs = minIndexWithElem' func (0, head xs) 0 xs

deleteMinTree :: (Ord a) => BinomialQueue a -> (BinomialTree a, BinomialQueue a)
deleteMinTree queue = (fromJust tree, newQueue)
  where (index, tree) = minIndexWithElem compareFunc queue
        compareFunc Nothing Nothing = EQ
        compareFunc Nothing x = GT
        compareFunc x Nothing = LT
        compareFunc (Just x) (Just y) = compare (root x) (root y)
        newQueue = (take index queue) ++ [Nothing] ++ (drop (index + 1) queue)

deleteMin :: (Ord a) => BinomialQueue a -> (a, BinomialQueue a)
deleteMin queue = (minElem, prune newQueue)
  where (tree, queueMinusTree) = deleteMinTree queue
        minElem = root tree
        rest = map Just $ children tree
        newQueue = meld queueMinusTree rest

prune :: (Ord a) => BinomialQueue a -> BinomialQueue a
prune = reverse . (dropWhile (== Nothing)) . reverse

queue = fromList [1, 4, 3, 3, 2, 4, 4, 6, 7, 0]

queueSort :: (Ord a) => [a] -> [a]
queueSort xs = deleteAll queue
  where queue = fromList xs
        deleteAll [] = []
        deleteAll queue = (x:rest)
          where (x, newQueue) = deleteMin queue
                rest = deleteAll newQueue

stringsTree :: (Show a) => BinomialTree a -> [String]
stringsTree tree = (show $ root tree):
               (map ("| " ++) $ concat $ map stringsTree $ children tree)

stringsQueue :: (Show a) => BinomialQueue a -> [String]
stringsQueue queue = map showTree queue
  where showTree Nothing = "Nothing\n\n"
        showTree (Just tree) = (concat $ map (++ "\n") $ stringsTree tree) ++ "\n"

printQueue :: (Show a) => BinomialQueue a -> IO ()
printQueue = putStrLn . concat . stringsQueue
