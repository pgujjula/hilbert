module Hilbert.PriorityQueue.BinomialQueue.BinomialTree
  ( BinomialTree
  , root
  , children
  , rank
  , singleton
  , link
  , debugPrintTree
  , fromList) where

data BinomialTree v p = BinomialTree
                          { root :: (v, p),
                            children :: [BinomialTree v p]}
    deriving (Show)

-- Finding the rank of a binomial tree with rank n is O(n) time
rank :: BinomialTree v p -> Int
rank = length . children

singleton :: v -> p -> BinomialTree v p
singleton val pty = BinomialTree (val, pty) []

link :: (Ord p) => BinomialTree v p 
                -> BinomialTree v p
                -> BinomialTree v p
link tree1@(BinomialTree (val1, pty1) children1)
     tree2@(BinomialTree (val2, pty2) children2) = 
       if pty1 < pty2
       then BinomialTree (val1, pty1) (tree2:children1)
       else BinomialTree (val2, pty2) (tree1:children2)

debugPrintTree :: (Show v, Show p) => BinomialTree v p -> IO ()
debugPrintTree = putStr . concat . debugStringTree
 where debugStringTree :: (Show v, Show p) => BinomialTree v p -> [String] 
       debugStringTree tree
         | rank tree == 0  = [(show $ root tree) ++ "\n"]
         | otherwise       = ((show $ root tree) ++ "\n"):
                             (map ("|  " ++) $ concat
                              $ map debugStringTree
                              $ children tree)

fromList :: (Ord p) => [(v, p)] -> BinomialTree v p
fromList = collapseTrees . (map (\(v, p) -> singleton v p))
  where collapseOnce (x1:x2:xs) = (link x1 x2):(collapseOnce xs)
        collapseOnce _ = []

        collapseTrees [x] = x
        collapseTrees xs = collapseTrees $ collapseOnce xs
