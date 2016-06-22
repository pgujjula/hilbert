{-|
    Module      : Hilbert.Prime.MillerRabin
    Description : The Miller-Rabin probabilistic primality test.
    Copyright   : (c) Preetham Gujjula, 2016
    License     : GPL-3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    The Miller-Rabin probabilistic primality test.
-}
module Hilbert.Prime.MillerRabin
  ( millerRabin
  , millerRabinWith
  ) where

import Hilbert.Modular (modPow)

-- The default bases used by the Miller Rabin test.
defaultBases :: (Integral a) => [a]
defaultBases = [2, 3, 5]

{-|
    @millerRabin n@ performs the Miller-Rabin test on @n@ with a
    default list of bases. Use @millerRabinWith@ to specify a different
    set of bases. This is a probabilistic prime test, so correctness is not
    guaranteed.
-}
millerRabin :: (Integral a) => a -> Bool
millerRabin n = millerRabinWith defaultBases n

{-|
    @millerRabinWith n testBases@ performs the Miller-Rabin test on
    @n@ with the bases @testBases@
-}
millerRabinWith :: (Integral a) => [a] -> a -> Bool
millerRabinWith testBases n =
  case millerRabinWith_preconditions n of
    Just b  -> b
    Nothing -> millerRabinWith_low_level testBases n

{-
   Check basic preconditions. Returns 'Just _' if n is was determined to be
   prime or not just by checking the preconditions. Returns 'Nothing' if n 
   passes the preconditions and the full test needs to be run.
-}
millerRabinWith_preconditions :: (Integral a) => a -> Maybe Bool
millerRabinWith_preconditions n
  | even n    = Just False
  | n < 2     = Just False
  | n == 3    = Just True
  | otherwise = Nothing

{-
   Called from millerRabinWith. Preconditions:
     * n > 3
     * n is odd
-}
millerRabinWith_low_level :: (Integral a) => [a] -> a -> Bool
millerRabinWith_low_level testBases n =
  not $ and $ map (\b -> isWitness b n) testBases

{-
   Check if 'a' is a witness to the compositness of 'n'. If true, then 'n' is 
   surely composite. If false, then either 'a' is a strong liar or 'n' is prime.
  
   'a' is a witness if
     (1) a^d is not congruent to 1 (mod n)
     (2) a^(2^r * d) is not congruent to -1 (mod n)
            for all 0 <= r <= s - 1
     where (s, d) = splitTwos n
-}
isWitness :: (Integral a) => a -> a -> Bool
isWitness a n = part1 && part2
  where part1 = adn /= 1
        part2 = and $ map (/= (n - 1)) $ take s $ iterate (\x -> x^2 `rem` n) adn
        adn = (modPow a d n)

        -- Check a specific 'r' value for part (2) of the test
        -- CAN MAKE THIS MORE EFFICIENT
        testR r = (modPow a (2^r * d) n) /= (n - 1)
        (s, d)  = splitTwos (n - 1)

-- Factor an integer into the form 2^r * d, with d odd
splitTwos :: (Integral a) => a -> (Int, a)
splitTwos n = if odd n
              then (0, n)
              else (r' + 1, d')
  where (r', d') = splitTwos (n `div` 2)
