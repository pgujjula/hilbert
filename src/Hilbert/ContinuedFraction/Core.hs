{-|
    Module      : Hilbert.ContinuedFraction.Core
    Description : Core continued fraction functionality.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Core continued fraction functionality.
-}
module Hilbert.ContinuedFraction.Core
  ( ContinuedFraction
  , mkPeriodic
  , mkAperiodic
  , repeatingPart
  , nonRepeatingPart
  , toList
  , isPeriodic
  , convergent
  ) where

import Data.Ratio

-- | Data type to represent continued fractions.
{-
   * Periodic xs ys represents a continued fraction with non-repeating part xs
     and repeating part ys. The list form of this fraction is xs ++ (cycle ys).
     ys should not be empty (checked), and both xs and ys should be finite
     (unchecked).
   * APeriodic xs represents a continued fraction that does not repeat. xs can
     be finite or infinite, but it should not be periodic after a while
     (unchecked).
-}
data ContinuedFraction a = Periodic [a] [a]
                         | APeriodic [a]

instance (Show a) => (Show (ContinuedFraction a)) where
   show (Periodic xs ys) = "mkPeriodic "  ++ (show xs) ++ " " ++ (show ys)
   show (APeriodic xs)   = "mkAperiodic " ++ (show xs)

{-|
    Create a new periodic continued fraction.

    >>> take 10 $ toList $ mkPeriodic [10, 4] [2, 7, 3]
    [10, 4, 2, 7, 3, 2, 7, 3, 2, 7]
-}
mkPeriodic :: [a] -> [a] -> ContinuedFraction a
mkPeriodic xs ys
  | null ys   = error $ "Repeating part cannot be null in"
                        ++ "periodic continued fraction."
  | otherwise = Periodic xs ys

{-|
    Create a new aperiodic continued fraction.

    >>> take 5 $ toList $ mkAperiodic [1..]
    [1, 2, 3, 4, 5]
-}
mkAperiodic :: [a] -> ContinuedFraction a
mkAperiodic xs = APeriodic xs

{-|
    Get the repeating part of a continued fraction, if it has one.

    >>> repeatingPart (sqrt 15)
    Just [1, 6]
    >>> repeatingPart (sqrt 100)
    Nothing
-}
repeatingPart :: ContinuedFraction a -> Maybe [a]
repeatingPart (APeriodic _) = Nothing
repeatingPart (Periodic _ xs) = Just xs

{-|
    Get the non-repeating part of a continued fraction. Both periodic and
    aperiodic fractions have non-repeating parts.

    >>> nonRepeatingPart (sqrt 1000)
    [31]
    >>> take 5 $ nonRepeatingPart (mkAperiodic [1, 2..])
    [1, 2, 3, 4, 5]
-}
nonRepeatingPart :: ContinuedFraction a -> [a]
nonRepeatingPart (APeriodic xs) = xs
nonRepeatingPart (Periodic xs _) = xs

{-|
    Get the list form of a continued fraction.

    >>> take 5 $ toList $ sqrt 10
    [3, 6, 6, 6, 6]
-}
toList :: ContinuedFraction a -> [a]
toList (Periodic xs ys) = xs ++ (cycle ys)
toList (APeriodic xs)   = xs

{-|
    @True@ if the list is periodic after a while.

    >>> isPeriodic (sqrt 10)
    True
    >>> isPeriodic (mkAperiodic [1, 2..])
    False
-}
isPeriodic :: ContinuedFraction a -> Bool
isPeriodic (Periodic _ _) = True
isPeriodic (APeriodic _) = False

{-|
    @convergent xs@ computes the rational number equal to the continued
    fraction represented by @xs@.

    For example, to compute convergents for the continued fraction for /e/,
    which is @[2, 1, 2, 1, 1, 4, 1, 1, 6, 1, ...]@,

@
e = mkAperiodic $ (2:) $ concat $ map (\x -> [1, x, 1]) [2, 4..]
approximation = convergent e 30
print (fromRational approximation)
@

    The output of this program is @2.718281828459045@, which is equal to /e/
    up to machine precision.

    Sometimes, the requested convergent is not available because the continued
    fraction is finite and not large enough. In this case, the last convergent,
    or the real value of the continued fraction, is returned.

    >>> convergent (mkPeriodic [] [1, 2]) 5
    3 % 2
    >>> convergent (mkPeriodic [] [1, 2]) 0
    0 % 1
-}
convergent :: (Integral a) => ContinuedFraction a -> Int -> Ratio a
convergent cfrac n = convFromList $ take n $ toList cfrac

-- Compute the convergent of a finite list representing a continued fraction.
convFromList :: (Integral a) => [a] -> Ratio a
convFromList [] = 0 % 1
convFromList [x] = x % 1
convFromList (x:xs) = (x % 1) + (1 / (convFromList xs))
