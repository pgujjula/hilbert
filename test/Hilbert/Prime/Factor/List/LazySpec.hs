module Hilbert.Prime.Factor.List.LazySpec
  ( main
  , spec
  ) where

import Test.Hspec (hspec, Spec, shouldBe, it, describe)

import Hilbert.Prime.Factor.List.Lazy (factorTo, factorToInf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "factorTo" $ do
    test limit1
    test limit2
  describe "factorToInf" $ do
    it ("correct up to " ++ (show limit2)) $ do
      let expected = [1..limit2]
      let actual = take limit2 $ map multiplyOut factorToInf
      sequence_ $ zipWith shouldBe actual expected

{-
   Supplementary data/functions
-}
multiplyOut :: [(Int, Int)] -> Int
multiplyOut = product . (map (uncurry (^)))


limit1 :: Int
limit1 = 100

limit2 :: Int
limit2 = 10000

test :: Int -> Spec
test limit = it ("works for " ++ (show limit)) $ do
  let expected = [1..limit]
  let actual = map multiplyOut (factorTo limit)
  sequence_ $ zipWith shouldBe actual expected
