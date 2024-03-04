{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.NumberTheory.QuadraticField (QuadraticField, (+:), sqrt) where

import Data.Ratio (denominator, numerator)
import Data.Type.Natural (KnownNat, Nat, SNat, sNat, toNatural)
import Math.NumberTheory.Power (square)
import Prelude hiding (sqrt)

data QuadraticField (d :: Nat) = QuadraticField !Rational !Rational
  deriving (Eq)

(+:) :: Rational -> Rational -> QuadraticField d
(+:) = QuadraticField

sqrt :: QuadraticField d
sqrt = QuadraticField 0 1

instance KnownNat d => Show (QuadraticField d) where
  show (QuadraticField a b) =
    showRational a
      ++ " + "
      ++ showRational b
      ++ "*sqrt @"
      ++ show d
    where
      d = toNatural (sNat :: SNat d)

instance KnownNat d => Num (QuadraticField d) where
  (QuadraticField x1 y1) + (QuadraticField x2 y2) =
    QuadraticField (x1 + x2) (y1 + y2)
  (QuadraticField x1 y1) - (QuadraticField x2 y2) =
    QuadraticField (x1 - x2) (y1 - y2)
  (QuadraticField x1 y1) * (QuadraticField x2 y2) =
    let d = fromIntegral . toNatural $ (sNat :: SNat d)
     in QuadraticField (x1 * x2 + y1 * y2 * d) (x1 * y2 + x2 * y1)
  abs qf@(QuadraticField x y) =
    case (compare x 0, compare y 0) of
      (LT, LT) -> negate qf
      (LT, EQ) -> negate qf
      (LT, GT) ->
        if square y * d >= square x
          then qf
          else negate qf
      (EQ, LT) -> negate qf
      (EQ, EQ) -> qf
      (EQ, GT) -> qf
      (GT, LT) ->
        if square x >= square y * d
          then qf
          else negate qf
      (GT, EQ) -> qf
      (GT, GT) -> qf
    where
      d = fromIntegral . toNatural $ (sNat :: SNat d)
  negate (QuadraticField x y) = QuadraticField (-x) (-y)
  signum qf@(QuadraticField x y)
    | x == 0 && y == 0 = 0
    | qf == abs qf = 1
    | otherwise = -1
  fromInteger n = QuadraticField (fromInteger n) 0

instance KnownNat d => Fractional (QuadraticField d) where
  fromRational r = QuadraticField r 0
  recip (QuadraticField x y) =
    let d = fromIntegral . toNatural $ (sNat :: SNat d)
        r = square x - d * square y
     in QuadraticField (x / r) (-y / r)

showRational :: Rational -> String
showRational r =
  let n = numerator r
      d = denominator r
   in if
          | d == 1 -> show n
          | n < 0 -> show n ++ "/" ++ show d
          | otherwise -> show n ++ "/" ++ show d
