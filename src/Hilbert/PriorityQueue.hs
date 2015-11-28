module Hilbert.PriorityQueue
  ( PriorityQueueADT
  , empty
  , Hilbert.PriorityQueue.ADT.null -- So as not to conflict with Prelude.null
  , size
  , insert
  , peekMin
  , peekMinWithPriority
  , deleteMin
  , deleteMinWithPriority
  , meld
  , fromList
  , MapQueue
  , PriorityQueue) where

import Hilbert.PriorityQueue.ADT
import Hilbert.PriorityQueue.MapQueue

type PriorityQueue = MapQueue
