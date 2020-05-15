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

newtype Poly a = Poly {unPoly :: IntMap a}
    deriving (Eq, Ord)

(!) :: (Num a) => Poly a -> Int -> a
(!) p n = fromMaybe 0 $ IntMap.lookup n $ unPoly p

degree :: Poly a -> Int
degree = maybe 0 fst . IntMap.lookupMax . unPoly

leadingCoefficient :: Num a => Poly a -> a
leadingCoefficient = maybe 0 snd . IntMap.lookupMax . unPoly

fromList :: [a] -> Poly a
fromList = Poly . IntMap.fromAscList . zip [0..]

fromAssocList :: [(Int, a)] -> Poly a
fromAssocList = Poly . IntMap.fromList

toList :: (Num a) => Poly a -> [a]
toList p = map (p !) [0..degree p]

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
