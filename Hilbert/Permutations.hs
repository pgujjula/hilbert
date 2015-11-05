module Hilbert.Permutations (permutations) where

extractOneAtTime :: [a] -> [(a, [a])]
extractOneAtTime [] = []
extractOneAtTime xs = map (\a -> (xs !! a, (take a xs) ++ (drop (a + 1) xs))) [0..length xs - 1]

permuteWithStarter :: (a, [a]) -> [[a]]
permuteWithStarter (a, list) = map (a:) (permutations list)

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = concat $ map permuteWithStarter $ extractOneAtTime xs
