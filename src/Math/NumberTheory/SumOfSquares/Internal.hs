{-# OPTIONS_HADDOCK hide #-}

module Math.NumberTheory.SumOfSquares.Internal
  ( sumOfSquaresNaive,
    sumOfSquaresUniqueNaive,
  )
where

import Math.NumberTheory.Roots (integerSquareRoot)

-- Find all @(a, b)@ such that @a^2 + b^2 == n@. Note that @a@ and @b@ are
-- allowed to be negative
sumOfSquaresNaive :: forall a. (Integral a) => a -> [(a, a)]
sumOfSquaresNaive 0 = [(0, 0)]
sumOfSquaresNaive n = do
  (a, b) <- sumOfSquaresQ1 n
  map (mul (a, b)) [(1, 0), (0, 1), (-1, 0), (0, -1)]

sumOfSquaresQ1 :: (Integral a) => a -> [(a, a)]
sumOfSquaresQ1 n = do
  (a, b) <- sumOfSquaresUniqueNaive n
  (a, b) : [(b, a) | a /= b && a > 0]

-- TODO: deduplicate
mul :: (Integral a) => (a, a) -> (a, a) -> (a, a)
mul (a, b) (c, d) = (a * c - b * d, a * d + b * c)

-- Find all @(a, b)@ such that @0 <= a <= b and @a^2 + b^2 == n@. Note that @a@
-- and @b@ are allowed to be negative.
sumOfSquaresUniqueNaive :: forall a. (Integral a) => a -> [(a, a)]
sumOfSquaresUniqueNaive n =
  if n < 0
    then []
    else go 0 sq
  where
    sq :: a
    sq = integerSquareRoot n

    go :: a -> a -> [(a, a)]
    go i j =
      if i > j
        then []
        else case compare (i * i + j * j) n of
          LT -> go (i + 1) j
          EQ -> (i, j) : go (i + 1) (j - 1)
          GT -> go i (j - 1)
