{-|
    Module      : Hilbert.PriorityQueue.Map
    Description : A priority queue implemention as a wrapper over 'Data.Map.Map'
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    A priority queue implemention as a wrapper over 'Data.Map.Map'
-}

{-# LANGUAGE TypeSynonymInstances #-}

module Hilbert.PriorityQueue.Map (MapQueue, getMap) where

import Data.List (sortBy)
import qualified Data.Map as Map
import Hilbert.PriorityQueue.ADT

-- | A priority queue implemention as a wrapper over 'Data.Map.Map'
newtype MapQueue v p = MapQueue (Map.Map p [v])
  deriving (Show)

-- | Get the underlying mapping from priorities to lists of values.
getMap :: MapQueue v p -> Map.Map p [v]
getMap (MapQueue m) = m

instance PriorityQueueADT MapQueue where
  empty = MapQueue Map.empty

  null = Map.null . getMap

  size = Map.size . getMap

  insert val pty (MapQueue map) =
    MapQueue $ Map.insertWith (++) pty [val] map

  peekMinWithPriority (MapQueue map) = (val, pty)
    where (pty, minList) = Map.findMin map
          val = head minList

  deleteMinWithPriority queue@(MapQueue map) = ((val, pty), queue')
    where (pty, minList) = Map.findMin map
          (val:rest) = minList
          queue' = MapQueue $ Map.updateMin updateFunc map
            where updateFunc _ = if Prelude.null rest
                                 then Nothing
                                 else Just rest

  meld (MapQueue map1) (MapQueue map2) =
    MapQueue $ Map.unionWith (++) map1 map2

  fromList = MapQueue
           . Map.fromList
           . group
           . sortBy (\(_, x) (_, y) -> compare x y)
    where group :: (Ord p) => [(v, p)] -> [(p, [v])]
          group [] = []
          group [(val, pty)] = [(pty, [val])]
          group ((val, pty):rest) = (pty, (val:otherVals)):otherPtys
            where recursed = group rest
                  otherVals = if (fst $ head recursed) == pty
                              then (snd $ head recursed)
                              else []
                  otherPtys = if (fst $ head recursed) == pty
                              then tail recursed
                              else recursed
