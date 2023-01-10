-- | Module      : Math.NumberTheory.Digit
--   Description : Figurate numbers.
--   Copyright   : (c) Preetham Gujjula, 2020
--   License     : GPL-3
--   Maintainer  : preetham.gujjula@gmail.com
--   Stability   : experimental
--
-- Figurate numbers (triangular, pentagonal, etc.)
module Math.NumberTheory.Figurate
  ( triangular,
    triangularN,
  )
where

import Data.List (scanl')

-- | Triangular numbers. Strict in the sense that evaluating a term result in the
--   evaluation of all the terms behind it.
--
--   >>> take 6 triangular
--   [0, 1, 3, 6, 10, 15]
triangular :: [Int]
triangular = scanl' (+) 0 [1 ..]

-- | The nth triangular number. Undefined for negative inputs.
--
--   >>> triangularN 3
--   6
--   >>> triangularN 0
--   0
triangularN :: (Integral a) => a -> a
triangularN n = n * (n + 1) `quot` 2
