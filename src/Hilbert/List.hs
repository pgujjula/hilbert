module Hilbert.List
  ( rmDups
  , rmDupsWith
  ) where


-- Removes duplicates from a sorted list
rmDups :: (Eq a) => [a] -> [a]
rmDups (x1:x2:xs) = if x1 == x2
                    then rmDups (x1:xs)
                    else x1:(rmDups (x2:xs))
rmDups xs = xs

-- Removes duplicates from a sorted list, 
-- using a custom comparison function
rmDupsWith :: (a -> a -> Bool) -> [a] -> [a]
rmDupsWith func (x1:x2:xs) = if func x1 x2
                             then rmDupsWith func (x1:xs)
                             else x1:(rmDupsWith func (x2:xs))
rmDupsWith func xs = xs
