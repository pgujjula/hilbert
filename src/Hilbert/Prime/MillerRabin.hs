module Hilbert.Prime.MillerRabin where

import Hilbert.Modular (modPow)

-- Miller-Rabin probabilistic prime test
-- Precondition: n >= 3
millerRabin :: (Integral a) => a -> [a] -> Bool
millerRabin n testBases = not $ any (baseTest n) testBases
  where baseTest n a = (part1 n a) && (part2 n a)
        part1 n a = (modPow a d n) /= 1
        part2 n a = all (\r -> (modPow a (2^r * d) n) /= (n - 1))
                        [0..s - 1]
        d = (n - 1) `div` (2^(numTwos (n - 1)))
        s = numTwos (n - 1)
        numTwos n = if odd n
                    then 0
                    else 1 + (numTwos (n `div` 2))
