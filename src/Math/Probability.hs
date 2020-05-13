module Math.Probability
    ( Distribution
    , fromList
    , certain
    , uniform
    , map
    , lift2
    , bind
    , toList
    ) where

import           Data.List  (genericLength)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Prelude    hiding (map, pure)

import           Data.Ratio ((%))

newtype Distribution a = Distribution {getMap :: Map a Rational}
    deriving (Show, Eq, Ord)

type Dist = Distribution

mkDist :: Map a Rational -> Distribution a
mkDist = Distribution

fromList :: (Ord a) => [(a, Rational)] -> Dist a
fromList xs = norm $ mkDist $ Map.fromList xs

certain :: a -> Dist a
certain x = mkDist (Map.singleton x 1)

uniform :: (Ord a) => [a] -> Dist a
uniform xs = mkDist $ Map.fromList $ zip xs (repeat $ 1 % genericLength xs)

norm :: Dist a -> Dist a
norm dist = mkDist $ Map.map (/ total) mp
  where
    total = sum mp
    mp = getMap dist

toList :: Dist a -> [(a, Rational)]
toList = Map.assocs . getMap

-- Functor-like
map :: (Ord b) => (a -> b) -> Dist a -> Dist b
map f = mkDist . Map.mapKeysWith (+) f . getMap

lift2 :: (Ord c) => (a -> b -> c) -> Dist a -> Dist b -> Dist c
lift2 f distA distB = mkDist $ Map.unionsWith (+) maps
  where
    maps = fmap mkMap eventsA
    mkMap (event, prob) = Map.map (*prob) $ Map.mapKeys (f event) mapB
    mapB = getMap distB
    eventsA = Map.assocs (getMap distA)

bind :: (Ord b) => Dist a -> (a -> Dist b) -> Dist b
bind dist f = mkDist $ Map.unionsWith (+) maps
  where
    mp = getMap dist
    events = Map.assocs mp
    maps = fmap mkMap events
    mkMap (event, prob) = Map.map (*prob) . getMap . f $ event