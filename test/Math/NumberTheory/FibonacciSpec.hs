module Math.NumberTheory.FibonacciSpec (spec) where

import Test.Hspec                  (Expectation, Spec, describe, it, shouldBe)
import Test.QuickCheck             (Gen, Property, choose, forAll, (===))

import Math.NumberTheory.Fibonacci (fibonacciModN, fibonacciN, fibonaccis,
                                    fibonaccisMod, lucasNumModN, lucasNumN,
                                    lucasNums, lucasNumsMod)

spec :: Spec
spec = do
    describe "fibonaccis"    fibonaccisSpec
    describe "fibonaccisMod" fibonaccisModSpec
    describe "fibonacciN"    fibonacciNSpec
    describe "fibonacciModN" fibonacciModNSpec

    describe "lucasNums"     lucasNumsSpec
    describe "lucasNumsMod"  lucasNumsModSpec
    describe "lucasNumN"     lucasNumNSpec
    describe "lucasNumModN"  lucasNumModNSpec

-- parameters
listLimit :: Int
listLimit = 100

modulusLimit :: Integer
modulusLimit = 10000

indexLimit :: Integer
indexLimit = 10^7

-- utilities
shouldBeList :: (Eq a, Show a) => [a] -> [a] -> Expectation
shouldBeList xs ys = take listLimit xs `shouldBe` take listLimit ys

equalsList :: (Eq a, Show a) => [a] -> [a] -> Property
equalsList xs ys = take listLimit xs === take listLimit ys

mkTests :: (Eq a, Show a) => (t -> [a]) -> (t -> [a]) -> (t -> Expectation, t -> Property)
mkTests f g = (test, qtest)
  where
    test x = f x `shouldBeList` g x
    qtest x = f x `equalsList` g x


fibonacciSpec :: Spec
fibonacciSpec =
    it "first 10" $
        take 10 fibonacci `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

fibonacciModSpec :: Spec
fibonacciModSpec = do
    let (test, qtest) = mkTests (\m -> map (`mod` m) $ fibonacciMod m)
                                (\m -> map (`mod` m) fibonacci)
    it "modulus 1" $ test 1
    it "modulus 2" $ test 2
    it "random modulus" $ forAll (choose (1, modulusLimit)) qtest
    it "fixed precision" $ forAll (choose (1, modulusLimit)) $ \m ->
        fibonacciMod (fromIntegral m :: Int) `shouldBeList` map fromIntegral (fibonacciMod m)

fibonacciNSpec :: Spec
fibonacciNSpec =
    it ("first " ++ show listLimit) $
        map fibonacciN [0..] `shouldBeList` fibonacci

fibonacciModNSpec :: Spec
fibonacciModNSpec = do
    let (test, qtest) = mkTests (\m -> map ((`mod` m) . fibonacciModN m) [0..])
                                (\m -> map (`mod` m) $ fibonacciMod m)
    it "modulus 1" $ test 1
    it "modulus 2" $ test 2
    it "random modulus" $ forAll (choose (1, modulusLimit)) qtest

    let gen :: Gen (Integer, Integer)
        gen = (,) <$> choose (1, modulusLimit) <*> choose (0, indexLimit)

    it "fixed precision" $ forAll gen $ \(m, n) ->
        toInteger (fibonacciModN (fromIntegral m :: Int) (fromIntegral n :: Int))
            `shouldBe` fibonacciModN m n

lucasNumSpec :: Spec
lucasNumSpec =
    it "first 10" $
        take 10 lucasNum `shouldBe` [2, 1, 3, 4, 7, 11, 18, 29, 47, 76]

lucasNumModSpec :: Spec
lucasNumModSpec = do
    let (test, qtest) = mkTests (\m -> map (`mod` m) $ lucasNumMod m)
                                (\m -> map (`mod` m) lucasNum)
    it "modulus 1" $ test 1
    it "modulus 2" $ test 2
    it "random modulus" $ forAll (choose (1, modulusLimit)) qtest
    it "fixed precision" $ forAll (choose (1, modulusLimit)) $ \m ->
        lucasNumMod (fromIntegral m :: Int) `shouldBeList` map fromIntegral (lucasNumMod m)

lucasNumNSpec :: Spec
lucasNumNSpec =
    it ("first " ++ show listLimit) $
        map lucasNumN [0..] `shouldBeList` lucasNum

lucasNumModNSpec :: Spec
lucasNumModNSpec = do
    let (test, qtest) = mkTests (\m -> map ((`mod` m) . lucasNumModN m) [0..])
                                (\m -> map (`mod` m) $ lucasNumMod m)
    it "modulus 1" $ test 1
    it "modulus 2" $ test 2
    it "random modulus" $ forAll (choose (1, modulusLimit)) qtest

    let gen :: Gen (Integer, Integer)
        gen = (,) <$> choose (1, modulusLimit) <*> choose (0, indexLimit)

    it "fixed precision" $ forAll gen $ \(m, n) ->
        toInteger (lucasNumModN (fromIntegral m :: Int) (fromIntegral n :: Int))
            `shouldBe` lucasNumModN m n
