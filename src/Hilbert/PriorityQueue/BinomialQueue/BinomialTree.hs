module Hilbert.PriorityQueue.BinomialQueue.BinomialTree
  ( BinomialTree
  , root
  , children
  , rank
  , singleton
  , link) where

data BinomialTree v p = BinomialTree
                          { root :: (v, p),
                            children :: [BinomialTree v p]}

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
