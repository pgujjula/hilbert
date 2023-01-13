module Test.Math.NumberTheory.ContinuedFraction.Core
  ( tests,
  )
where

import Control.Monad (forM_)
import Data.Ratio ((%))
import Math.NumberTheory.ContinuedFraction
  ( convergent,
    convergents,
    isPeriodic,
    mkAperiodic,
    mkPeriodic,
    nonRepeatingPart,
    repeatingPart,
    toList,
  )
import Test.HUnit.Lang (assertFailure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Util (throwsException)

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.ContinuedFraction.Core"
    [ mkPeriodicTests,
      repeatingPartTests,
      nonRepeatingPartTests,
      toListTests,
      isPeriodicTests,
      convergentTests,
      convergentsTests
    ]

mkPeriodicTests :: TestTree
mkPeriodicTests =
  testGroup
    "mkPeriodic"
    [ testCase "mkPeriodic _ [] throws an error" $
        throwsException (mkPeriodic ([1, 2, 3] :: [Int]) [])
    ]

repeatingPartTests :: TestTree
repeatingPartTests =
  testGroup
    "repeatingPart"
    [ testCase "aperiodic continued fraction" $
        repeatingPart (mkAperiodic [1, 2, 3]) @?= Nothing,
      testCase "periodic continued fraction" $
        repeatingPart (mkPeriodic [1, 2] [3, 4])
          @?= Just [3, 4]
    ]

nonRepeatingPartTests :: TestTree
nonRepeatingPartTests =
  testGroup
    "nonRepeatingPart tests"
    [ testCase "periodic continued fraction" $
        nonRepeatingPart (mkPeriodic [1, 2] [3, 4]) @?= [1, 2],
      testCase "aperiodic continued fraction" $
        nonRepeatingPart (mkAperiodic [1, 2, 3]) @?= [1, 2, 3]
    ]

toListTests :: TestTree
toListTests =
  testGroup
    "toList tests"
    [ testCase "aperiodic continued fractions" $
        toList (mkAperiodic [1, 2, 3]) @?= [1, 2, 3],
      testCase "periodic continued fractions" $
        take 8 (toList (mkPeriodic [1, 2] [3, 4]))
          @?= [1, 2, 3, 4, 3, 4, 3, 4]
    ]

isPeriodicTests :: TestTree
isPeriodicTests =
  testGroup
    "isPeriodic tests"
    [ testCase "periodic continued fractions" $
        isPeriodic (mkPeriodic [1, 2] [3, 4]) @?= True,
      testCase "aperiodic continued fractions" $
        isPeriodic (mkAperiodic [1, 2, 3]) @?= False
    ]

convergentTests :: TestTree
convergentTests =
  testGroup
    "convergent tests"
    [ testCase "approximates e well" $ do
        let e = mkAperiodic $ (2 :) $ concatMap (\x -> [1, x, 1]) [2, 4 ..]
        let shouldBeCloseTo x y =
              if abs (x - y) < 10 ** (-20)
                then return ()
                else
                  assertFailure $
                    show x ++ " not close enough to " ++ show y
        fromRational (convergent e 100) `shouldBeCloseTo` 2.718281828459045,
      testCase "periodic fractions" $ do
        convergent (mkPeriodic [1, 2] [3, 4]) 0 @?= (0 % 1)
        convergent (mkPeriodic [1, 2] [3, 4]) 4 @?= (43 % 30)
        convergent (mkPeriodic [1, 2] [3, 4]) 6 @?= (599 % 418)
        convergent (mkPeriodic [] [3, 4]) 3 @?= (42 % 13),
      testCase "aperiodic fractions" $ do
        convergent (mkAperiodic [1, 2, 3]) 0 @?= (0 % 1)
        convergent (mkAperiodic [1, 2, 3]) 2 @?= (3 % 2)
        convergent (mkAperiodic [1, 2, 3]) 4 @?= (10 % 7)
        convergent (mkAperiodic []) 5 @?= (0 % 1)
    ]

convergentsTests :: TestTree
convergentsTests =
  testGroup
    "convergents"
    [ testCase "matches behavior of convergent" $ do
        let e = mkAperiodic $ (2 :) $ concatMap (\x -> [1, x, 1]) [2, 4 ..]
        let cfracs =
              [ e,
                mkPeriodic [1, 2] [3, 4],
                mkPeriodic [] [3, 4],
                mkAperiodic [1, 2, 3],
                mkAperiodic []
              ]
        let maxNumTerms = 5 :: Int
        forM_ cfracs $ \cfrac -> do
          let numTerms = 1 + length (take maxNumTerms (toList cfrac))
          take numTerms (convergents cfrac)
            @?= fmap (convergent cfrac) (take numTerms [0 ..]),
      testCase "ends gracefully on finite continued fractions" $ do
        length (convergents (mkAperiodic [])) @?= 1
        length (convergents (mkAperiodic [1, 2, 3])) @?= 4
    ]
