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
  , MapQueue
  , getMap
  , PriorityQueue) where

import Hilbert.PriorityQueue.ADT
import Hilbert.PriorityQueue.Map

{-| The default priority queue implementation is MapQueue -}
type PriorityQueue = MapQueue
