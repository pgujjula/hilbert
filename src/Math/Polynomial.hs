{-| Module      : Math.Polynomial
    Description : Polynomial type and functions
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Polynomial type and functions.
-}

module Math.Polynomial
    ( Poly
    , (!)
    , degree
    , leadingCoefficient
    , fromList
    , fromAssocList
    , toList
    , toAssocList
    ) where

import           Data.Function    (on)
import           Data.IntMap      (IntMap)
import qualified Data.IntMap      as IntMap
import           Data.Maybe       (fromMaybe)

import           Data.Composition ((.:))

{-| A type for univariate polynomials over type @a@. -}
newtype Poly a = Poly {unPoly :: IntMap a}
    deriving (Eq, Ord)

{-| The @poly ! n@ coefficient of @x^n@ in the polynomial.

    >>> poly = fromList [1, 2, 1]   -- 1 + 2x + x^2
    >>> poly ! 2
    1
    >>> poly ! 3
    0
    >>> poly ! (-1)
    0
-}
(!) :: (Num a) => Poly a -> Int -> a
(!) p n = fromMaybe 0 $ IntMap.lookup n $ unPoly p

{-| The degree of the polynomial. The degree of the 0 polynomial is taken to be
    @-1@.
-}
degree :: Poly a -> Int
degree = maybe 0 fst . IntMap.lookupMax . unPoly

{-| The leading coefficient of the polynomial. The leading coefficient of the
    zero polynomial is taken to be 0.
-}
leadingCoefficient :: Num a => Poly a -> a
leadingCoefficient = maybe 0 snd . IntMap.lookupMax . unPoly

{-| Construct a polynomial from a list of coefficients, in order of increasing
    degree.
-}
fromList :: [a] -> Poly a
fromList = Poly . IntMap.fromDistinctAscList . zip [0..]

{-| Construct a polynomial from a list of (exponent, coefficient) terms. If an
    exponent is repeated, the latest instance is taken. No checks are performed
    for negative exponents.
-}
fromAssocList :: [(Int, a)] -> Poly a
fromAssocList = Poly . IntMap.fromList

{-| Get a list of coefficients from a polynomial.

    >>> poly = fromAssocList [(5, 1), (0, 2)]     -- x^5 + 2
    >>> toList poly
    [2, 0, 0, 0, 0, 1]
-}
toList :: (Num a) => Poly a -> [a]
toList p = map (p !) [0..degree p]

{-| Get a list of (exponent, coefficient) associations, in order of ascending
    exponent.

    >>> p1 = [1, 1]   -- 1 + x
    >>> p2 = [-1, 1]  -- 1 - x
    >>> toAssocList (p1 * p2)
    [(0, -1), (2, 1)]
-}
toAssocList :: Poly a -> [(Int, a)]
toAssocList = IntMap.toAscList . unPoly

trim :: (Eq a, Num a) => Poly a -> Poly a
trim p
    | degree p == 0             = p
    | leadingCoefficient p /= 0 = p
    | otherwise                 = trim (Poly . IntMap.deleteMax . unPoly $ p)

instance (Show a, Num a) => Show (Poly a) where
    show p = "fromList " ++ show (toList p)

instance (Eq a, Num a) => Num (Poly a) where
    (+)         = trim . Poly .: IntMap.unionWith (+) `on` unPoly
    (*) p q     = fromList $ map mkCoeff [0..dp + dq]
      where
        dp = degree p
        dq = degree q
        mkCoeff i = sum
                  $ map (\k -> (p ! k) * (q ! (i - k)))
                        [max 0 (i - dq)..min i dp]

    abs p       = Poly
                . IntMap.map (* (signum $ leadingCoefficient p))
                . unPoly
                $ p

    signum      = Poly . IntMap.singleton 0 . signum . leadingCoefficient
    fromInteger = Poly . IntMap.singleton 0 . fromInteger
    negate      = Poly . IntMap.map negate . unPoly
