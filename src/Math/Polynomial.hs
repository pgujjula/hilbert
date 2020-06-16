{-| Module      : Math.Polynomial
    Description : Polynomial type and functions
    Copyright   : (c) Preetham Gujjula, 2020
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    Polynomial type and functions.
-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Data.Function      (on)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe         (fromMaybe)

import           Data.Composition   ((.:))

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
degree = maybe (-1) fst . IntMap.lookupMax . unPoly

{-| The leading coefficient of the polynomial. The leading coefficient of the
    zero polynomial is taken to be 0.
-}
leadingCoefficient :: Num a => Poly a -> a
leadingCoefficient = maybe 0 snd . IntMap.lookupMax . unPoly

removeZeros :: (Num a, Eq a) => Poly a -> Poly a
removeZeros = Poly . IntMap.filter (/= 0) . unPoly

{-| Construct a polynomial from a list of coefficients, in order of increasing
    degree.
-}
fromList :: (Num a, Eq a) => [a] -> Poly a
fromList = removeZeros
         . Poly
         . IntMap.fromDistinctAscList . zip [0..]
         . reverse . dropWhile (== 0) . reverse

{-| Construct a polynomial from a list of (exponent, coefficient) terms. If an
    exponent is repeated, the latest instance is taken. No checks are performed
    for negative exponents.
-}
fromAssocList :: (Num a, Eq a) => [(Int, a)] -> Poly a
fromAssocList = removeZeros . Poly . IntMap.fromList

{-| Get a list of coefficients from a polynomial.

    >>> poly = fromAssocList [(5, 1), (0, 2)]     -- x^5 + 2
    >>> toList poly
    [2, 0, 0, 0, 0, 1]
-}
toList :: Num a => Poly a -> [a]
toList p@(Poly mp) =
    if IntMap.null mp
    then []
    else map (p !) [0..degree p]

{-| Get a list of (exponent, coefficient) associations, in order of ascending
    exponent.

    >>> p1 = [1, 1]   -- 1 + x
    >>> p2 = [-1, 1]  -- 1 - x
    >>> toAssocList (p1 * p2)
    [(0, -1), (2, 1)]
-}
toAssocList :: Poly a -> [(Int, a)]
toAssocList = IntMap.toAscList . unPoly

instance (Show a, Num a) => Show (Poly a) where
    show p = "fromList " ++ show (toList p)

shift :: Int -> Poly a -> Poly a
shift x = Poly . IntMap.mapKeysMonotonic (+ x) . unPoly

scale :: Num a => a -> Poly a -> Poly a
scale x = Poly . IntMap.map (*x) . unPoly

mul :: forall a. (Num a, Eq a) => Poly a -> Poly a -> Poly a
mul p q = removeZeros
        . unsafeAdd $ fmap ($ q) funcs
  where
    funcs :: [Poly a -> Poly a]
    funcs = fmap (\(e, c) -> shift e . scale c) $ toAssocList p

    unsafeAdd :: [Poly a] -> Poly a
    unsafeAdd = Poly . IntMap.unionsWith (+) . fmap unPoly

instance forall a. (Eq a, Num a) => Num (Poly a) where
    (+) = (Poly . IntMap.fromDistinctAscList . reverse)
        .: unionAdd `on` (reverse . IntMap.toList . unPoly)
      where
        unionAdd :: [(Int, a)] -> [(Int, a)] -> [(Int, a)]
        unionAdd xs [] = xs
        unionAdd [] ys = ys
        unionAdd ((e1, c1):xs) ((e2, c2):ys) =
            case compare e1 e2 of
                GT -> (e1, c1) : unionAdd xs ((e2, c2):ys)
                LT -> (e2, c2) : unionAdd ((e1, c1):xs) ys
                EQ -> let s = c1 + c2
                       in if s == 0
                          then unionAdd xs ys
                          else (e1, s) : unionAdd xs ys


    (*) p q = if degree p <= degree q then mul p q else mul q p

    abs p = Poly
          . IntMap.map (* (signum $ leadingCoefficient p))
          . unPoly
          $ p

    signum p@(Poly mp)
        | IntMap.null mp = p
        | otherwise      = Poly . IntMap.singleton 0
                         . signum . leadingCoefficient $ p

    fromInteger x
        | x == 0    = Poly IntMap.empty
        | otherwise = Poly . IntMap.singleton 0 . fromInteger $ x

    negate      = Poly . IntMap.map negate . unPoly
