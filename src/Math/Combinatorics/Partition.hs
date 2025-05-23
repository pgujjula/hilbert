-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-| Module      : Math.Combinatorics.Partition
    Description : Counting partitions of an integer.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : BSD-3-Clause
    Maintainer  : libraries@mail.preetham.io
    Stability   : experimental

Counting partitions of an integer.
-}

module Math.Combinatorics.Partition (numPartitions, partitions) where

import Data.Chimera (memoizeFix)
import Data.List (foldl')

{-| The number of partitions of n. For example, since we can write 4 as
    1 + 1 + 1 + 1, 1 + 1 + 2, 1 + 3, 2 + 2, and 4, we have

    >>> numPartitions 4
    5

    Some special cases:

    >>> numPartitions 0
    1
    >>> numPartitions (-1)  -- or any negative number
    0
-}
numPartitions :: (Integral a) => a -> a
numPartitions n
    | n < 0     = 0
    | n ==  0   = 1
    | otherwise = fromIntegral $ part (fromIntegral n)

part :: Word -> Integer
part = memoizeFix partFix

-- use the partition function formula -- see Wikipedia
partFix :: (Word -> Integer) -> Word -> Integer
partFix p n
  | n == 0    = 1
  | otherwise = foldl' (+) 0 $ zipWith (*) weights $ map p recursiveArgs
  where
    weights :: [Integer]
    weights = cycle [1, 1, -1, -1]

    -- recursively call the partition function with these arguments
    recursiveArgs :: [Word]
    recursiveArgs = map fromIntegral $ takeWhile (>= 0) $ map (fromIntegral n -) offsets

    -- alternate positive and negative pentagonal numbers
    offsets :: [Int]
    offsets = map pentagonal $ alternate [1..] [-1, -2..]
      where
        pentagonal :: (Integral a) => Int -> a
        pentagonal k = k' * (3*k' - 1) `div` 2
          where
            k' = fromIntegral k

        alternate :: [a] -> [a] -> [a]
        alternate (x:xs) (y:ys) = x : y : alternate xs ys
        alternate _ _           = error "finite list"

{-| Enumerate the partitions of n. 

    >>> partitions 4
    [[4],[3,1],[2,2],[2,1,1],[1,1,1,1]]
-}
partitions :: Integral a => a -> [[a]]
partitions n = partitionsWithMax n n

partitionsWithMax :: Integral a => a -> a -> [[a]]
partitionsWithMax m n
 | n < 0     = []
 | n == 0    = [[]]
 | otherwise =
     [m, m-1..1] >>= \i ->
       fmap (i:) (partitionsWithMax i (n - i))
