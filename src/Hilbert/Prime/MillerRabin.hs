{-|
Module      : Hilbert.Prime.MillerRabin
Description : The Miller-Rabin probabilistic primality test.
Copyright   : (c) Preetham Gujjula, 2016
License     : GPL-3
Maintainer  : preetham.gujjula@gmail.com
Stability   : experimental

The Miller-Rabin probabilistic primality test.

-}

module Hilbert.Prime.MillerRabin (millerRabin, millerRabinWith) where

import Hilbert.Modular (modPow)

defaultBases :: (Integral a) => [a]
defaultBases = [2, 3, 5]

{-| @millerRabin n@ performs the Miller-Rabin test on @n@ with a
    default list of bases. Use @millerRabinWith@ to specify a different
    set of bases. This is a probabilistic prime test, so correctness is not 
    guaranteed.

    > Precondition: n >= 3
-}
millerRabin :: (Integral a) => a -> Bool
millerRabin n = millerRabinWith defaultBases n

{-| @millerRabinWith n testBases@ performs the Miller-Rabin test on
    @n@ with the bases @testBases@

    > Precondition: n >= 3
-}
millerRabinWith :: (Integral a) => [a] -> a -> Bool
millerRabinWith testBases n = not $ any (baseTest n) testBases
  where baseTest n a = (part1 n a) && (part2 n a)
        part1 n a = (modPow a d n) /= 1
        part2 n a = all (\r -> (modPow a (2^r * d) n) /= (n - 1))
                        [0..s - 1]
        d = (n - 1) `div` (2^(numTwos (n - 1)))
        s = numTwos (n - 1)
        numTwos n = if odd n
                    then 0
                    else 1 + (numTwos (n `div` 2))
