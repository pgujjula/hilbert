{-| Module      : Math.NumberTheory.Digit
    Description : Figurate numbers.
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

Figurate numbers (triangular, pentagonal, etc.)
-}
module Math.NumberTheory.Figurate
    ( triangular
    , triangularN
    ) where

triangular :: [Int]
triangular = scanl1 (+) [0..]

triangularN :: (Integral a) => a -> a
triangularN n = n * (n + 1) `div` 2
