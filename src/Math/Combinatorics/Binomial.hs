module Math.Combinatorics.Binomial
    ( factorial
    , choose
    , permute
    ) where

import Data.List (foldl', genericTake)

{-| The factorial of a number. Undefined behavior for negative inputs.

    >>> factorial 5
    120
-}
factorial :: (Integral a) => a -> a
factorial n = foldl' (*) 1 [1..n]

{-| The binomial coefficient. @choose n k@ is defined as 0 for any @n < 0@ or
    @k > n@ or @k < 0@.

    >>> 5 `choose` 2
    10
-}
choose :: (Integral a) => a -> a -> a
choose n k
    | n < 0          = 0
    | k > n          = 0
    | k < 0          = 0
    | k > n `quot` 2 = choose' n (n - k)
    | otherwise      = choose' n k

-- no preconditions are checked
choose' :: (Integral a) => a -> a -> a
choose' n k = foldl' (\i (p, q) -> i * p `quot` q) 1
            $ genericTake k
            $ zip [n, n - 1..] [1..]

{-| Number of permutations groups of size @k@, selected from a group of size
    @n@. @permute n k@ is defined as 0 for any @n < 0@ or @k > n@ or @k < 0@.

    >>> 5 `permute` 2
    20
-}
permute :: (Integral a) => a -> a -> a
permute n k
    | n < 0     = 0
    | k > n     = 0
    | k < 0     = 0
    | otherwise = foldl' (*) 1 $ genericTake k [n, n - 1..]
