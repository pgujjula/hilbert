module Hilbert.Modular where

-- Computes base^exp (mod modulus)
-- Precondition: modulus > 0, exp >= 0, base >= 0
-- modPow 0 0 _ = 1
modPow :: (Integral a) => a -> a -> a -> a
modPow base exp modulus | exp == 0 = 1
modPow base exp modulus | exp == 1 = base `rem` modulus
modPow base exp modulus | abs base >= modulus
  = modPow (base `rem` modulus) exp modulus
modPow base exp modulus | exp `rem` 2 == 0
  = ((modPow base redExp modulus)^2) `rem` modulus
    where redExp = exp `div` 2
modPow base exp modulus
  = (base * (modPow base redExp modulus)^2) `rem` modulus
     where redExp = (exp - 1) `div` 2
