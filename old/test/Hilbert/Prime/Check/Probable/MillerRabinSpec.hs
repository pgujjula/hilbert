module Hilbert.Prime.Check.Probable.MillerRabinSpec
  ( main
  , spec
  ) where

import Hilbert.Prime.Check.Probable.MillerRabin (millerRabinWith)
import Hilbert.Prime.Check.TrialDivision (isPrime)

import Test.Hspec;
import Test.QuickCheck;
import Test.Hspec.QuickCheck;
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit.Base (Test(TestCase), assertEqual)

import Data.Int;
import qualified Data.Set as Set;
import Data.Set ((\\))

main = hspec spec

{-
   Parameters
-}
numberOfTests = 1000

{-
   Tests
-}
spec = modifyMaxSuccess (\_ -> numberOfTests) $
           describe "millerRabinWith" $ do
             it "works on numbers large numbers using bases 2, 3, 5, 7" $ do
               forAll smallGen $ \x -> (millerRabinWith [2, 3, 5, 7] x) == (isPrime x)
             testBase 2
             testBase 3
             testBase 5
             testBase 7
             testAllBases

{-
   millerRabinWith tests
-}
testBase p = it ("correctly finds pseudoprimes in base " ++ (show p)) $ do
               let range = [4..strongPseudoLimit]
               let actual = filter (\x -> (millerRabinWith [p] x) &&
                                   (not $ isPrime x)) range
               let expected = strongPseudo p
               actual `shouldMatchList` expected

testAllBases = it "using more bases eliminates the pseudoprimes of few bases" $ do
                sequence_ $ zipWith3 assertEqual messages expected actual

    where bases :: [Int]
          bases = [2, 3, 5, 7]

          pseudoprimes :: [Int]
          pseudoprimes = concat $ map strongPseudo bases

          expected :: [Bool]
          expected = map (\_ -> False) pseudoprimes

          actual :: [Bool]
          actual = map (millerRabinWith bases) pseudoprimes

          makeMessage :: Int -> String
          makeMessage n = "millerRabinWith " ++ (show bases) ++ " " ++ (show n)

          messages :: [String]
          messages = map makeMessage pseudoprimes

{- 
   Supplementary data
-}

-- Strong pseudprimes less than 100000 in bases 2, 3, 5, 7, according to OEIS.
strongPseudoLimit = 100000

strongPseudo :: (Integral a) => Int -> [a]
strongPseudo 2 = [2047, 3277, 4033, 4681, 8321, 15841, 29341, 42799,
                  49141, 52633, 65281, 74665, 80581, 85489, 88357, 90751]

strongPseudo 3 = [121, 703, 1891, 3281, 8401, 8911, 10585, 12403, 16531,
                  18721, 19345, 23521, 31621, 44287, 47197, 55969, 63139,
                  74593, 79003, 82513, 87913, 88573, 97567]

strongPseudo 5 = [781, 1541, 5461, 5611, 7813, 13021, 14981, 15751, 24211,
                  25351, 29539, 38081, 40501, 44801, 53971, 79381]

strongPseudo 7 = [25, 325, 703, 2101, 2353, 4525, 11041, 14089, 20197, 29857,
                  29891, 39331, 49241, 58825, 64681, 76627, 78937, 79381, 87673,
                  88399, 88831]

-- Pseudoprimes for all bases (this is not used at the moment)
allBasePseudo :: (Integral a) => [a]
allBasePseudo = [3215031751, 118670087467, 307768373641, 315962312077,
                 354864744877, 457453568161, 528929554561, 546348519181,
                 602248359169, 1362242655901, 1871186716981, 2152302898747,
                 2273312197621, 2366338900801, 3343433905957, 3461715915661,
                 3474749660383, 3477707481751, 4341937413061, 4777422165601]

smallGen :: Gen Integer
smallGen = choose (4, 3 * (10^9))
