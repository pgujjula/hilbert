module Hilbert.Prime.BailliePSW where

import Hilbert.Prime.LucasLehmer (lucasLehmer)
import Hilbert.Prime.MillerRabin (millerRabin)

-- Baillie-PSW primality test
-- Precondition: n >= 3
bailliePSW :: (Integral a) => a -> Bool
bailliePSW n = (millerRabin n [2]) && (lucasLehmer n)
