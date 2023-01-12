{-# LANGUAGE ImportQualifiedPost #-}

-- | Module      : Math.Probability
--   Description : Discrete probability distributions
--   Copyright   : (c) Preetham Gujjula, 2020
--   License     : BSD-3-Clause
--   Maintainer  : preetham.gujjula@gmail.com
--   Stability   : experimental
--
--   Discrete probability distributions.
module Math.Probability
  ( Distribution,
    fromList,
    certain,
    uniform,
    toList,
    toMap,
    prob,
    expectedValue,
    variance,
    map,
    lift2,
    bind,
  )
where

import Data.Functor ((<&>))
import Data.List (foldl', genericLength)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Prelude hiding (map)

-- | Discrete probablity distribution, with probabilities of type @p@ and events
--   of type @a@, backed by a 'Map' from @a@ to @p@.
newtype Distribution a p = Distribution {getMap :: Map a p}
  deriving (Eq, Ord)

instance (Show a, Show p) => Show (Distribution a p) where
  show = show . getMap

normalize :: Fractional p => Map a p -> Map a p
normalize mp = Map.map (/ total) mp
  where
    total = sum mp

-- | Convert a list of events and weights to a probability distribution. The
--   weights are normalized to sum to 1. Only the last appearance of each event
--   in the list is considered.
fromList :: (Ord a, Fractional p) => [(a, p)] -> Distribution a p
fromList = Distribution . normalize . Map.fromList

-- | Probability distribution where there is only one event, with probability
--   1.
certain :: Num p => a -> Distribution a p
certain x = Distribution (Map.singleton x 1)

-- | Distribution where each event is equally likely.
--
--   >>> uniform [1, 2, 3, 4]
--   fromList [(1,1 % 4),(2,1 % 4),(3,1 % 4),(4,1 % 4)]
--   >>> uniform [1, 2, 3, 1]
--   fromList [(1,1 % 2),(2,1 % 4),(3,1 % 4)]
uniform :: (Ord a, Fractional p) => [a] -> Distribution a p
uniform xs =
  Distribution $
    Map.fromListWith (+) $
      zip xs $
        repeat (fromRational $ 1 % genericLength xs)

-- | The probability of a specific event.
--
--   >>> die = uniform [1..6] :: Distribution Int Rational
--   >>> prob die 4
--   1 % 6
prob :: (Ord a, Num p) => Distribution a p -> a -> p
prob (Distribution mp) a = fromMaybe 0 $ Map.lookup a mp

-- | The expected value of a distribution.
--
--   >>> expectedValue (uniform [1..6 :: Rational])
--   7 % 2
expectedValue :: (Fractional a, Real p) => Distribution a p -> a
expectedValue =
  foldl' (+) 0
    . fmap (\(e, p) -> e * (fromRational . toRational $ p))
    . toList

-- | The variance of a distribution.
--
--   >>> variance (uniform [1..6 :: Rational])
--   35 % 12
variance :: (Fractional a, Ord a, Real p) => Distribution a p -> a
variance dist =
  expectedValue (map (^ (2 :: Int)) dist)
    - expectedValue dist ^ (2 :: Int)

-- | Convert a distribution to a list of events and probabilities. The weights
--   sum to 1.
toList :: Distribution a p -> [(a, p)]
toList = Map.assocs . getMap

-- | Convert a distribution to a map from events to probabilities.
toMap :: Distribution a p -> Map a p
toMap = getMap

-- | Functor-like map function.
map :: (Ord b, Num p) => (a -> b) -> Distribution a p -> Distribution b p
map f = Distribution . Map.mapKeysWith (+) f . getMap

-- | Applicative-like lifting function.
lift2 ::
  (Ord c, Num p) =>
  (a -> b -> c) ->
  Distribution a p ->
  Distribution b p ->
  Distribution c p
lift2 f dist1 dist2 =
  Distribution . Map.unionsWith (+) $
    toList dist1 <&> \(x, px) ->
      Map.map (* px) . Map.mapKeys (f x) . getMap $ dist2

-- | Monad-like bind function.
bind ::
  (Ord b, Num p) =>
  Distribution a p ->
  (a -> Distribution b p) ->
  Distribution b p
bind dist f =
  Distribution . Map.unionsWith (+) $
    toList dist <&> \(event, p) ->
      Map.map (* p) $ getMap $ f event
