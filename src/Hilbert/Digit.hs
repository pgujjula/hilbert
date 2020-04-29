{-|
    Module      : Hilbert.Digit
    Description : Functions to handle the digits of integers.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Functions to handle the digits of integers.
-}

module Hilbert.Digit
  ( numDigits
  , sumDigits
  , toDigits
  , fromDigits
  ) where

{-|
    @numDigits n@ is the number of digits in @n@.

    >>> numDigits 2938475
    7
    >>> numDigits 0
    1
    >>> numDigits (-38417)
    5
-}
numDigits :: (Integral a) => a -> Int
numDigits = length . show . toInteger . abs

{-|
    @sumDigits n@ is the sum of the digits in @n@.

    >>> sumDigits 2938475
    38
    >>> sumDigits 0
    0
    >>> sumDigits (-12)
    3
-}
sumDigits :: (Integral a) => a -> Int
sumDigits = sum . toDigits . abs

{-|
    @toDigits n@ is a list of all the digits in @n@.

    __Precondition:__ @n@ must be nonnegative.

    >>> toDigits 2938475
    [2, 9, 3, 8, 4, 7, 5]
    >>> toDigits 0
    [0]
-}
toDigits :: (Integral a) => a -> [Int]
toDigits = reverse . toDigits'
  where
  -- Generate the digits in reverse order, which is easier.
    toDigits' :: (Integral a) => a -> [Int]
    toDigits' n | n < 10 = [fromIntegral n]
    toDigits' n = first:rest
      where first = fromIntegral $ n `rem` 10
            rest = toDigits' (n `div` 10)

{-|
    @fromDigits xs@ is converts a list of digits @xs@ to an integer.

    __Precondition:__ All elements of @xs@ are in @[0..9]@.

    >>> fromDigits [1, 7, 2, 9]
    1729
    >>> fromDigits []
    0
    >>> fromDigits [0, 0, 0]
    0
    >>> fromDigits [0, 4, 2]
    42
-}
fromDigits :: (Integral a) => [a] -> Integer
fromDigits = fromDigits' . reverse
  where
  -- Computing the number is easier if the digits are given in reverse
  fromDigits' [] = 0
  fromDigits' (x:xs) = if (x < 0) || (x > 9)
                       then error $ (show (toInteger x))
                                    ++ " must be between 0 and 9 inclusive"
                       else 10*(fromDigits' xs) + (fromIntegral x)
