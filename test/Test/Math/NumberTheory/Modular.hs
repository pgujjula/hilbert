module Test.Math.NumberTheory.Modular (tests) where

import Math.NumberTheory.Modular (egcd)
import Test.Hspec () -- for instance Testable Assertion
import Test.QuickCheck (Gen, choose, forAll)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Math.NumberTheory.Modular" [egcdTest]

-- The maximum size of any argument in these tests
limit :: Integer
limit = 10000

egcdTest :: TestTree
egcdTest =
  testGroup
    "egcd tests"
    [ testCase "egcd 0 0 == (0, (0, 0))" $
        egcd 0 0 @?= (0, (0, 0)),
      testProperty "egcd m 0 == (abs m, (signum m, 0)) for all m" $
        forAll gen $
          \m -> egcd m 0 @?= (abs m, (signum m, 0)),
      testProperty "egcd 0 n == (abs n, (0, signum n)) for all n" $
        forAll gen $
          \n -> egcd 0 n @?= (abs n, (0, signum n)),
      testProperty "works for arbitrary inputs" $
        forAll ((,) <$> gen <*> gen) $ \(m, n) -> do
          let (g, (a, b)) = egcd m n
          g @?= gcd m n
          a * m + b * n @?= g
    ]
  where
    gen :: Gen Integer
    gen = choose (1, limit)
