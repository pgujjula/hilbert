{-| Module      : Math.NumberTheory.Digit
    Description : Handle the digits of numbers
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

Handle the digits of numbers.
-}
module Math.NumberTheory.Digit
    ( numDigits
    , sumDigits
    , fromDigits
    , toDigits
    ) where

import Data.Char (chr, ord)

ordZero :: Int
ordZero = ord '0'

{-| @numDigits n@ is the number of digits in @n@.

    >>> numDigits 2938475
    7
    >>> numDigits 0
    1
    >>> numDigits (-38417)
    5
-}
numDigits :: (Integral a) => a -> Int
numDigits = length . show . toInteger . abs

{-| @sumDigits n@ is the sum of the digits in @n@.

    >>> sumDigits 2938475
    38
    >>> sumDigits 0
    0
    >>> sumDigits (-12)
    3
-}
sumDigits :: (Integral a) => a -> Int
sumDigits = sum . toDigits . abs

{-| Convert a list of digits to an integer.

    __Precondition:__ All elements of the input are in @[0..9]@.

    >>> fromDigits [1, 7, 2, 9]
    1729
    >>> fromDigits []
    0
    >>> fromDigits [0, 0, 0]
    0
    >>> fromDigits [0, 4, 2]
    42
-}
fromDigits :: (Integral a) => [Int] -> a
fromDigits = compute . validate
  where
    compute = fromInteger . read . map fromDigit . pad
      where
        fromDigit x = chr (x + ordZero)
        pad = (0 :)  -- for empty input lists

    validate xs
        | not (all (`elem` [0..9]) xs) = error errorMessage
        | otherwise                    = xs
      where
        errorMessage = "Math.NumberTheory.Digit.fromDigits: All elements of the input "
                    ++ "must be in the range [0..9]."

{-| Generate the list of digits in the input.

    __Precondition:__ The input must be nonnegative. The domain is restricted so
    that the invariant @(fromDigits . toDigits) n == n@ holds for any @n@ in the
    domain of @toDigits@. This wouldn't be possible if we allowed things like
    @toDigits (-12) == [1, 2]@.

    >>> toDigits 2338475
    [2, 3, 3, 8, 4, 7, 5]
    >>> toDigits 0
    [0]
-}
toDigits :: (Integral a) => a -> [Int]
toDigits = map toDigit . show . toInteger . validate
  where
    toDigit x = ord x - ordZero
    validate n
      | n < 0     = error "Math.NumberTheory.Digit.toDigits: Input must be nonnegative."
      | otherwise = n
