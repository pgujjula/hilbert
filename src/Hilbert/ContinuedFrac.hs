module Hilbert.ContinuedFrac (convergent, continuedFrac) where

import Data.Maybe (fromJust)
import Data.List (find, findIndices)
import Hilbert.Square (isSquare)
import Data.Ratio

data Irrational a = Irrational a a a a
                deriving (Show, Eq)

reciprocal :: (Integral a) => Irrational a -> Irrational a
reciprocal (Irrational a b c d) =
          Irrational
              (newa `div` g)
              b
              (newc `div` g)
              (newd `div` g)
  where g = gcd (gcd newa newc) newd
        newa = a*d
        newc = -c*d
        newd = (a^2)*b - (c^2)

flr :: (Integral a) => Irrational a -> a
flr (Irrational a b c d) = floor $ ((f a)*sqrt(f b) + (f c)) / (f d)
  where f = fromIntegral

minus :: (Integral a) => Irrational a -> a -> Irrational a
minus (Irrational a b c d) n = Irrational (newa `div` g)
                                          b
                                          (newc `div` g)
                                          (newd `div` g)
  where newa = a
        newc = (c - d*n)
        newd = d
        g = gcd (gcd newa newc) newd


convergent :: (Integral a) => [a] -> a -> Ratio a
convergent _ 0 = 0
convergent (x:xs) n = 1 / ((fromIntegral x) + (convergent xs (n - 1)))

continuedFrac n | isSquare n = Nothing
continuedFrac n = Just $ ((map fst $ take first cfrac),
               (map fst $ drop first $ take second cfrac))
  where cfrac = continuedFrac' n
        repeat = fromJust $ findRepeat cfrac
        (first:second:_) = findIndices (== repeat) cfrac
        continuedFrac' x = generate (a0, b0)
          where a0 = floor $ sqrt $ fromIntegral x
                b0 = reciprocal (Irrational 1 x (-a0) 1)
                generate (a, b) = (a, b):(generate (flr b, reciprocal(b `minus` flr(b))))
        findRepeat list = findRepeat' [] list
          where findRepeat' old (n:new) = 
                  case find (== n) old of
                    Nothing -> findRepeat' (n:old) new
                    otherwise -> Just n

