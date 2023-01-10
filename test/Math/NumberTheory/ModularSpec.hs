module Math.NumberTheory.ModularSpec (spec) where

import Control.Exception (evaluate)
import Data.Int (Int8)
import Data.Maybe (fromJust, isJust)
import Math.NumberTheory.Modular (egcd)
import Test.Hspec
  ( Spec,
    anyException,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
    shouldThrow,
  )
import Test.QuickCheck (Gen, choose, forAll, suchThat)

spec :: Spec
spec = do
  describe "egcd" egcdSpec

-- The maximum size of any argument in these tests
limit :: Integer
limit = 10000

egcdSpec :: Spec
egcdSpec = do
  let gen :: Gen Integer
      gen = choose (1, limit)

  it "egcd 0 0 == (0, (0, 0))" $
    egcd 0 0 `shouldBe` (0, (0, 0))
  it "egcd m 0 == (abs m, (signum m, 0)) for all m" $
    forAll gen $
      \m -> egcd m 0 `shouldBe` (abs m, (signum m, 0))
  it "egcd 0 n == (abs n, (0, signum n)) for all n" $
    forAll gen $
      \n -> egcd 0 n `shouldBe` (abs n, (0, signum n))
  it "works for arbitrary inputs" $
    forAll ((,) <$> gen <*> gen) $ \(m, n) -> do
      let (g, (a, b)) = egcd m n
      g `shouldBe` gcd m n
      a * m + b * n `shouldBe` g
