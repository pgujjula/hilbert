module Hilbert.List (rmDups) where

-- Removes duplicates from a sortedList
rmDups :: (Eq a) => [a] -> [a]
rmDups (x1:x2:xs) = if x1 == x2
                    then rmDups (x1:xs)
                    else x1:(rmDups (x2:xs))
rmDups xs = xs
