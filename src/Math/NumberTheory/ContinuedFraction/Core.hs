-- | Module      : Math.NumberTheory.ContinuedFraction.Core
--   Description : Core continued fraction functionality.
--   Copyright   : (c) Preetham Gujjula, 2016
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
--   Core continued fraction functionality.
module Math.NumberTheory.ContinuedFraction.Core
  ( ContinuedFraction,
    mkPeriodic,
    mkAperiodic,
    repeatingPart,
    nonRepeatingPart,
    toList,
    isPeriodic,
    convergent,
    convergents,
  )
where

import Data.Ratio

-- | Data type to represent continued fractions.
data ContinuedFraction a
  = {- Periodic xs ys represents a continued fraction with non-repeating part xs
       and repeating part ys. The list form of this fraction is xs ++ (cycle ys).
       ys should not be empty (checked), and both xs and ys should be finite
       (unchecked).
    -}
    Periodic [a] [a]
  | {- APeriodic xs represents a continued fraction that does not repeat. xs can
       be finite or infinite, but it should not be periodic after a while
       (unchecked).
    -}
    APeriodic [a]
  deriving (Eq)

instance (Show a) => (Show (ContinuedFraction a)) where
  show (Periodic xs ys) = "mkPeriodic " ++ show xs ++ " " ++ show ys
  show (APeriodic xs) = "mkAperiodic " ++ show xs

-- | Create a new periodic continued fraction.
--
--   __Preconditions:__ In @mkPeriodic xs ys@,
--
--     * @xs@ and @ys@ should be finite (unchecked).
--     * @ys@ should not be empty (checked).
--
--   >>> take 10 $ toList $ mkPeriodic [10, 4] [2, 7, 3]
--   [10, 4, 2, 7, 3, 2, 7, 3, 2, 7]
mkPeriodic :: [a] -> [a] -> ContinuedFraction a
mkPeriodic xs ys
  | null ys =
      error $
        "mkPeriodic: Repeating part cannot be null in periodic "
          ++ "continued fraction."
  | otherwise = Periodic xs ys

-- | Create a new aperiodic continued fraction.
--
--   __Precondition:__ In @mkAperiodic xs@, @xs@ can be finite or infinite, but
--   it should not be periodic after a while (unchecked).
--
--   >>> take 5 $ toList $ mkAperiodic [1..]
--   [1, 2, 3, 4, 5]
mkAperiodic :: [a] -> ContinuedFraction a
mkAperiodic = APeriodic

-- | Get the repeating part of a continued fraction, if it has one.
--
--   __Preconditions:__ None.
--
--   >>> repeatingPart (sqrt 15)
--   Just [1, 6]
--   >>> repeatingPart (sqrt 100)
--   Nothing
repeatingPart :: ContinuedFraction a -> Maybe [a]
repeatingPart (APeriodic _) = Nothing
repeatingPart (Periodic _ xs) = Just xs

-- | Get the non-repeating part of a continued fraction.
--
--   __Preconditions:__ None.
--
--   >>> nonRepeatingPart (sqrt 1000)
--   [31]
--   >>> take 5 $ nonRepeatingPart (mkAperiodic [1, 2..])
--   [1, 2, 3, 4, 5]
nonRepeatingPart :: ContinuedFraction a -> [a]
nonRepeatingPart (APeriodic xs) = xs
nonRepeatingPart (Periodic xs _) = xs

-- | Convert a continued fraction to a list of terms.
--
--   __Preconditions:__ None.
--
--   >>> take 5 $ toList $ sqrt 10
--   [3, 6, 6, 6, 6]
toList :: ContinuedFraction a -> [a]
toList (Periodic xs ys) = xs ++ cycle ys
toList (APeriodic xs) = xs

-- | @True@ if the list is periodic after a while.
--
--   __Preconditions:__ None.
--
--   >>> isPeriodic (sqrt 10)
--   True
--   >>> isPeriodic (mkAperiodic [1, 2..])
--   False
isPeriodic :: ContinuedFraction a -> Bool
isPeriodic (Periodic _ _) = True
isPeriodic (APeriodic _) = False

-- | @convergent cfrac n@ computes the @n@th convergent of @cfrac@.
--
--   For example, to compute convergents for the continued fraction for /e/,
--   which is @[2, 1, 2, 1, 1, 4, 1, 1, 6, 1, ...]@,
--
-- @
-- e = mkAperiodic $ (2:) $ concat $ map (\x -> [1, x, 1]) [2, 4..]
-- approximation = convergent e 30
-- print (fromRational approximation)
-- @
--
--   The output of this program is @2.718281828459045@, which is equal to /e/
--   up to machine precision.
--
--   Sometimes, the requested convergent is not available because the continued
--   fraction is finite and not large enough. In this case, the last convergent,
--   or the real value of the continued fraction, is returned.
--
--   __Precondition:__ In @convergent cfrac n@, @n@ must be nonnegative.
--
--   >>> convergent (mkPeriodic [] [1, 2]) 5
--   3 % 2
--   >>> convergent (mkPeriodic [] [1, 2]) 0
--   0 % 1
convergent :: Integral a => ContinuedFraction a -> Int -> Ratio a
convergent cfrac n = convFromList $ take n $ toList cfrac

-- Compute the convergent of a finite list representing a continued fraction.
convFromList :: Integral a => [a] -> Ratio a
convFromList [] = 0 % 1
convFromList [x] = x % 1
convFromList (x : xs) = (x % 1) + (1 / convFromList xs)

-- | Generate the convergents of a given continued fraction @cfrac@ faster than
--   calling @fmap (convergent cfrac) [0..]@
--
--   >>> take 5 $ convergents (mkPeriodic [] [1, 2])
--   asfd
convergents :: forall a. Integral a => ContinuedFraction a -> [Ratio a]
convergents cfrac = trunc $ (0 :) $ tail $ zipWith (%) numers denoms
  where
    numers = 1 : head as : zipWith (+) numers (zipWith (*) (tail numers) (tail as))
    denoms = 0 : 1 : zipWith (+) denoms (zipWith (*) (tail denoms) (tail as))
    as = toList cfrac
    trunc xs = zipWith const xs (0 : as)
