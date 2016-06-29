{-|
    Module      : Hilbert.PriorityQueue
    Description : Pure priority queue implementations
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Pure priority queue implementations.
-}
module Hilbert.PriorityQueue
  ( PriorityQueueADT(..)
  , DefaultQueue
  , MapQueue
  , ListQueue
  ) where

import Hilbert.PriorityQueue.ADT
import Hilbert.PriorityQueue.Map
import Hilbert.PriorityQueue.List

-- | The default priority queue implementation is MapQueue.
type DefaultQueue = MapQueue
