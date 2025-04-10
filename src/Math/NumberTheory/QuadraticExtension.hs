-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE PatternSynonyms #-}

-- | Module      : Math.NumberTheory.QuadraticExtension
--   Description : Handle the digits of numbers
--   Copyright   : (c) Preetham Gujjula, 2024
--   License     : BSD-3-Clause
--   Maintainer  : libraries@mail.preetham.io
--   Stability   : experimental
--
-- Extensions of semirings, rings, and fields by \(\sqrt{d}\). That is, given a
-- semiring, ring, or field \(A\) and a square-free positive integer \(d\),
-- this module provides the semiring, ring, or field
-- \(\{x + y \sqrt{d}\mid x, y \in A\}\).
module Math.NumberTheory.QuadraticExtension
  ( QuadraticExtension (..),
    pattern Sqrt,
  )
where

import Data.Semiring
  ( Ring,
    Semiring,
    fromNatural,
    one,
    plus,
    times,
    zero,
  )
import Data.Semiring qualified as Semiring
import Data.Type.Natural (KnownNat, Nat, SNat, fromSNat, sNat)
import GHC.Stack (HasCallStack)

-- | The quadratic extension of the set represented by @a@ by \(\sqrt{d}\).
-- That is, @QuadraticExtension \@d x y@ represents \(x + y \sqrt{d}\).
data QuadraticExtension (d :: Nat) a = QuadraticExtension !a !a
  deriving (Eq)

-- | The value \(\sqrt{d}\). Useful for writing expressions like
-- @1 + 2 * Sqrt \@3@.
pattern Sqrt :: forall d a. (Eq a, Num a) => QuadraticExtension (d :: Nat) a
pattern Sqrt <- QuadraticExtension 0 1
  where
    Sqrt = QuadraticExtension 0 1

instance
  forall d a.
  (KnownNat d, Show a) =>
  Show (QuadraticExtension d a)
  where
  showsPrec prec (QuadraticExtension a b) =
    let d = fromSNat (sNat :: SNat d)
        wrapParen s =
          if prec > 6
            then (("(" ++ s ++ ")") ++)
            else (s ++)
     in wrapParen
          ( showsPrec 6 a
              . (" + " ++)
              . showsPrec 7 b
              . (" * Sqrt @" ++)
              . showsPrec 7 d
              $ ""
          )

signumError :: (HasCallStack) => a
signumError = error "signum outside of [-1, 0, 1]"

instance (KnownNat d, Num a, Eq a) => Ord (QuadraticExtension d a) where
  compare x y =
    case signum (x - y) of
      1 -> GT
      0 -> EQ
      -1 -> LT
      _ -> signumError

instance
  forall d a.
  (KnownNat d, Eq a, Num a) =>
  Num (QuadraticExtension d a)
  where
  QuadraticExtension a1 b1 + QuadraticExtension a2 b2 =
    QuadraticExtension (a1 + a2) (b1 + b2)
  QuadraticExtension a1 b1 - QuadraticExtension a2 b2 =
    QuadraticExtension (a1 - a2) (b1 - b2)
  QuadraticExtension a1 b1 * QuadraticExtension a2 b2 =
    let d = fromIntegral (fromSNat (sNat :: SNat d))
     in QuadraticExtension
          (a1 * a2 + b1 * b2 * d)
          (a1 * b2 + a2 * b1)
  negate (QuadraticExtension a b) = QuadraticExtension (-a) (-b)
  abs x =
    case signum x of
      1 -> x
      0 -> x
      -1 -> negate x
      _ -> signumError
  signum (QuadraticExtension a b) =
    let d :: a
        d = fromIntegral (fromSNat (sNat :: SNat d))
     in case (signum a, signum b) of
          (0, 0) -> 0
          (0, 1) -> 1
          (0, -1) -> -1
          (1, 0) -> 1
          (1, 1) -> 1
          (1, -1) -> QuadraticExtension (signum (a * a - b * b * d)) 0
          (-1, 0) -> -1
          (-1, 1) -> QuadraticExtension (signum (b * b * d - a * a)) 0
          (-1, -1) -> -1
          _ -> signumError
  fromInteger x = QuadraticExtension (fromInteger x) 0

instance
  forall d a.
  (KnownNat d, Eq a, Fractional a) =>
  Fractional (QuadraticExtension d a)
  where
  fromRational r = QuadraticExtension (fromRational r) 0
  recip (QuadraticExtension a b) =
    let d :: a
        d = fromIntegral (fromSNat (sNat :: SNat d))
        denom = a * a - b * b * d
        a' = a / denom
        b' = -b / denom
     in QuadraticExtension a' b'

instance (KnownNat d, Semiring a) => Semiring (QuadraticExtension d a) where
  plus (QuadraticExtension a1 b1) (QuadraticExtension a2 b2) =
    QuadraticExtension (a1 `plus` a2) (b1 `plus` b2)
  times (QuadraticExtension a1 b1) (QuadraticExtension a2 b2) =
    let d = fromNatural (fromSNat (sNat :: SNat d))
     in QuadraticExtension
          (plus (times a1 a2) (times d (times b1 b2)))
          (plus (times a1 b2) (times a2 b1))
  zero = QuadraticExtension zero zero
  one = QuadraticExtension one zero
  fromNatural n = QuadraticExtension (fromNatural n) zero

instance (KnownNat d, Ring a) => Ring (QuadraticExtension d a) where
  negate (QuadraticExtension a b) =
    QuadraticExtension (Semiring.negate a) (Semiring.negate b)
