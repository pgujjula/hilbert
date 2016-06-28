module Hilbert.Prime.Factor.SingleSpec
 ( main
 , spec
 ) where

import Test.Hspec (hspec, describe, Spec, it, shouldBe)
import Hilbert.Prime.Factor.Single (factor)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "factor" $ do
         it ("correct for all integers up to " ++ (show testLimit)) $ do
           let expected = map (multiplyOut . factor) [1..]
           let actual   = [1..]
           sequence_ $ take testLimit $ zipWith shouldBe actual expected

{-
   Supplementary data/functions
-}
multiplyOut :: [(Int, Int)] -> Int
multiplyOut = product . (map (\(p, e) -> p^e))

testLimit :: Int
testLimit = 10000
