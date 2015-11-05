module Hilbert.Poly where
import Data.Ratio
import Data.List (foldl1, find)
import Data.Maybe (fromJust)

data Poly a = Poly [a]
  deriving (Show)

instance (Eq a) => Eq (Poly a) where
  (Poly xs) == (Poly ys) = (xs == ys)

instance (Num a) => Num (Poly a) where
  (Poly xs) + (Poly ys) = Poly (combineList xs ys)
  (Poly xs) * (Poly ys) = polyMul (Poly xs) (Poly ys)
  abs (Poly xs)         = (Poly xs)*(Poly [abs $ last xs])
  signum (Poly xs)      = Poly [signum $ last xs]
  fromInteger x         = Poly [fromInteger x]
  negate poly           = (Poly [-1])*poly


combineList :: (Num a) => [a] -> [a] -> [a]
combineList xs [] = xs
combineList [] ys = ys
combineList (x:xs) (y:ys) = (x + y):(combineList xs ys)

polyMul (Poly xs) (Poly [c]) = Poly $ map (*c) xs
polyMul (Poly xs) (Poly (c:rest)) = (polyMul (Poly xs) (Poly [c]))
                                  + (polyMul (Poly (0:xs)) (Poly rest))

scalarMul :: (Num a) => a -> Poly a -> Poly a
scalarMul n (Poly xs) = Poly (map (*n) xs)

evaluate :: (Num a) => Poly a -> a -> a
evaluate (Poly [c])  _ = c
evaluate (Poly (x:xs)) n = x + (n * (evaluate (Poly xs) n))

degree :: Poly a -> Int
degree (Poly xs) = (length xs) - 1

-- Lagrange interpolation
lagrangeInterpolation points = answer
  where
    xs = map fst points
    ys = map snd points

    polyxs xs = map (\x -> Poly [-x, 1]) xs

    extractOne [x] = [[]]
    extractOne (x:xs) = xs:(map (x:) $ extractOne xs)

    numerators = map (\x -> foldl1 (*) x) $map polyxs $ extractOne xs
    denominators = zipWith (\x poly -> evaluate poly x) xs numerators

    getList (Poly xs) = xs
    ells = zipWith (\n d -> Poly $ map (%d) (getList n))
                   numerators denominators

    terms = zipWith scalarMul (map fromInteger ys) ells

    answer = if length points == 1
             then Poly [(snd $ head points) % 1]
             else foldl1 (+) terms
