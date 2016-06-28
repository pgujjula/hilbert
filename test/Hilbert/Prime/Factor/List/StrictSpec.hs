module Hilbert.Prime.Factor.List.StrictSpec
  ( main
  , spec
  ) where

import Test.Hspec (hspec, Spec, describe, it, shouldBe)

import Hilbert.Prime.Factor.List.Strict (factorTo')

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "factorTo" $ do
    test limit1
    test limit2

{-
   Supplementary data/functions
-}
multiplyOut :: [(Int, Int)] -> Int
multiplyOut = product . (map (\(p, e) -> p^e))

limit1 :: Int
limit1 = 100

limit2 :: Int
limit2 = 10000

test :: Int -> Spec
test limit = it ("works for " ++ (show limit)) $ do
  let expected = [1..limit]
  let actual = map multiplyOut (factorTo' limit)
  sequence_ $ zipWith shouldBe actual expected
