module Math.NumberTheory.ModularSpec (spec) where

import Control.Exception         (evaluate)
import Data.Int                  (Int8)
import Data.Maybe                (fromJust, isJust)
import Test.Hspec                (Spec, anyException, describe, it, shouldBe,
                                  shouldSatisfy, shouldThrow)
import Test.QuickCheck           (Gen, choose, forAll, suchThat)

import Math.NumberTheory.Modular (egcd, modInv, modPow)

spec :: Spec
spec = do
    describe "modPow" modPowSpec
    describe "egcd" egcdSpec
    describe "modInv" modInvSpec

-- The maximum size of any argument in these tests
limit :: Integer
limit = 10000

modPowSpec :: Spec
modPowSpec = do
    let modGen :: Gen Integer
        modGen = choose (-limit, limit) `suchThat` (/= 0)

    let expGen :: Gen Integer
        expGen = choose (0, limit) `suchThat` (/= 0)

    let baseGen :: Gen Integer
        baseGen = choose (-limit, limit)

    it "modulus of 0 throws error" $
        evaluate (modPow 3 4 0) `shouldThrow` anyException
    it "negative exponent throws error" $
        evaluate (modPow 3 (-4) 10) `shouldThrow` anyException
    it "modPow 0 0 ±1 == 0" $ do
        modPow 0 0 1 `shouldBe` 0
        modPow 0 0 (-1) `shouldBe` 0
    it "modPow 0 0 m == 1 for any m > 1" $
        forAll (modGen `suchThat` (\m -> abs m > 1)) $ \m ->
            modPow 0 0 m `shouldBe` 1
    it "modPow 0 b m == 0 for any b > 0" $
        forAll ((,) <$> (expGen `suchThat` (> 0)) <*> modGen) $ \(b, m) ->
            modPow 0 b m `shouldBe` 0
    it "modPow a 0 m == 1 for any m > 1" $ do
        let gen = (,) <$> baseGen <*> (modGen `suchThat` (\m -> abs m > 1))
        forAll gen $ \(a, m) ->
            modPow a 0 m `shouldBe` 1
    it "modPow a b ±1 == 0" $
        forAll ((,) <$> baseGen <*> expGen) $ \(a, b) -> do
            modPow a b 1 `shouldBe` 0
            modPow a b (-1) `shouldBe` 0
    it "works for general a, b, m" $
        forAll ((,,) <$> baseGen <*> expGen <*> modGen) $ \(a, b, m) ->
            modPow a b m `shouldBe` ((a^b) `mod` abs m)

    it "works for fixed precision a b m" $ do
        let smallBaseGen :: Gen Int8
            smallBaseGen = choose (minBound, maxBound)

        let smallExpGen :: Gen Int8
            smallExpGen = choose (0, maxBound) `suchThat` (/= 0)

        let smallModGen :: Gen Int8
            smallModGen = choose (minBound, maxBound) `suchThat` (/= 0)

        let gen :: Gen (Int8, Int8, Int8)
            gen = (,,) <$> smallBaseGen <*> smallExpGen <*> smallModGen

        forAll gen $ \(a, b, m) -> do
            let (a', b', m') = (toInteger a, toInteger b, toInteger m)
            toInteger (modPow a b m) `shouldBe` modPow a' b' m'

egcdSpec :: Spec
egcdSpec = do
    let gen :: Gen Integer
        gen = choose (1, limit)

    it "egcd 0 0 == (0, (0, 0))" $
        egcd 0 0 `shouldBe` (0, (0, 0))
    it "egcd m 0 == (abs m, (signum m, 0)) for all m" $
        forAll gen $ \m -> egcd m 0 `shouldBe` (abs m, (signum m, 0))
    it "egcd 0 n == (abs n, (0, signum n)) for all n" $
        forAll gen $ \n -> egcd 0 n `shouldBe` (abs n, (0, signum n))
    it "works for arbitrary inputs" $
        forAll ((,) <$> gen <*> gen) $ \(m, n) -> do
            let (g, (a, b)) = egcd m n
            g `shouldBe` gcd m n
            a*m + b*n `shouldBe` g

modInvSpec :: Spec
modInvSpec = do
    let baseGen :: Gen Integer
        baseGen = choose (-limit, limit)

    let modGen :: Gen Integer
        modGen = choose (-limit, limit) `suchThat` (/= 0)

    let relPrimeGen :: Gen (Integer, Integer)
        relPrimeGen = ((,) <$> baseGen <*> modGen)
                      `suchThat` (\(b, m) -> gcd b m == 1)

    let notRelPrimeGen :: Gen (Integer, Integer)
        notRelPrimeGen = ((,) <$> baseGen <*> modGen)
                      `suchThat` (\(b, m) -> gcd b m > 1)

    it "no inverses modulo 0" $
        forAll baseGen $ \b -> modInv b 0 `shouldBe` Nothing

    it "no inverses if base shares factor with exponent" $
        forAll notRelPrimeGen $ \(b, m) -> modInv b m `shouldBe` Nothing

    it "inverse exists if base doesn't share factor with exponent" $
        forAll relPrimeGen $ \(b, m) -> do
            let maybeInv = modInv b m
            maybeInv `shouldSatisfy` isJust
            let inv = fromJust maybeInv
            inv `shouldSatisfy` (>= 0)
            inv `shouldSatisfy` (< abs m)
            (inv*b) `mod` abs m `shouldBe` 1
