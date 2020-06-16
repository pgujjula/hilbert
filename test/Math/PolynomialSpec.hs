{-# LANGUAGE TypeApplications #-}
module Math.PolynomialSpec (spec) where

import Control.Monad   (forM_, replicateM)
import Test.Hspec      (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, choose, forAll, sample, suchThat, (===))

import Math.Polynomial (Poly, degree, fromAssocList, fromList,
                        leadingCoefficient, toAssocList, toList, (!))

maxDegree :: Int
maxDegree = 10

maxCoefficient :: Integer
maxCoefficient = 10

polyGen :: Gen (Poly Integer)
polyGen = do
    d <- choose (1, maxDegree)
    fmap fromList $ replicateM d
                  $ choose (-maxCoefficient, maxCoefficient)

zeroPoly :: Poly Integer
zeroPoly = fromList []

spec :: Spec
spec = do
    describe "(!)" bangSpec
    describe "degree" degreeSpec
    describe "leadingCoefficient" leadingCoefficientSpec
    describe "fromList/toList" fromToListSpec
    describe "fromAssocList" fromAssocListSpec
    describe "toAssocList" toAssocListSpec

    describe "(+)" addSpec
    describe "(*)" productSpec
    describe "abs" absSpec
    describe "signum" signumSpec
    describe "abs/signum invariant" absSignumInvariantSpec
    describe "fromInteger" fromIntegerSpec
    describe "negate" negateSpec

bangSpec :: Spec
bangSpec = do
    it "works for 0 polynomial" $
        forM_ [-5..5] $ \i -> zeroPoly ! i `shouldBe` 0
    it "works for small polynomial" $ do
        let poly = fromList [1, 0, 2]
        poly ! (-1) `shouldBe` 0
        poly ! 0 `shouldBe` 1
        poly ! 1 `shouldBe` 0
        poly ! 2 `shouldBe` 2
        poly ! 3 `shouldBe` 0

degreeSpec :: Spec
degreeSpec = do
    it "works for 0 polynomial" $
        degree zeroPoly `shouldBe` (-1)
    it "works for small polynomials" $ do
        degree (fromList [3, 0, 3]) `shouldBe` 2
        degree (fromList [3]) `shouldBe` 0
        degree (fromList [0, 3, 0, 0]) `shouldBe` 1

leadingCoefficientSpec :: Spec
leadingCoefficientSpec = do
    it "works for zero polynomial" $
        leadingCoefficient zeroPoly `shouldBe` 0
    it "works for small polynomials" $ do
        leadingCoefficient (fromList [6, 7, 8]) `shouldBe` 8
        leadingCoefficient (fromList [6, 7, 0]) `shouldBe` 7
        leadingCoefficient (fromList [1]) `shouldBe` 1
        leadingCoefficient (fromList [0]) `shouldBe` 0

fromToListSpec :: Spec
fromToListSpec = do
    it "works for zero polynomial" $ do
        toList (fromList []) `shouldBe` []
        toList (fromList [0]) `shouldBe` []
    it "works for small polynomials" $ do
        let fl = fromList @Integer
        toList (fl [1, 0, 2]) `shouldBe` [1, 0, 2]
        toList (fl [1, 0, 2, 0]) `shouldBe` [1, 0, 2]
        toList (fl [0]) `shouldBe` []
        toList (fl [0, 1]) `shouldBe` [0, 1]

fromAssocListSpec :: Spec
fromAssocListSpec = do
    it "works for zero polynomial" $ do
        fromAssocList [] `shouldBe` zeroPoly
        fromAssocList [(0, 0)] `shouldBe` zeroPoly
        fromAssocList [(2, 0), (3, 0)] `shouldBe` zeroPoly
        fromAssocList [(2, 1), (3, 0), (2, 0)] `shouldBe` zeroPoly
    it "works for small polynomials" $ do
        fromAssocList [(5, 1), (0, -2)]
            `shouldBe` fromList [-2, 0, 0, 0, 0, 1]
        fromAssocList [(0, 3), (5, 1), (0, -2)]
            `shouldBe` fromList [-2, 0, 0, 0, 0, 1]

toAssocListSpec :: Spec
toAssocListSpec = do
    it "works for zero polynomial" $
        toAssocList zeroPoly `shouldBe` []
    it "works for small polynomials" $ do
        toAssocList (fromList [-2, 0, 0, 0, 0, 1]) `shouldBe` [(0, -2), (5, 1)]
        toAssocList (fromList [-2, 0, 0, 1, 0, 0]) `shouldBe` [(0, -2), (3, 1)]

addSpec :: Spec
addSpec = do
    it "works for polynomials adding to zero" $ do
        zeroPoly + zeroPoly `shouldBe` zeroPoly
        fromList [1, 2] + fromList [-1, -2] `shouldBe` zeroPoly
    it "works for arbitrary polynomials adding to zero" $
        forAll polyGen $ \poly -> poly - poly `shouldBe` zeroPoly
    it "works for small polynomials" $ do
        fromList [1, 2, 3] + fromList [4, 5, 6]
            `shouldBe` fromList [5, 7, 9]
        fromList [1, 2, -3] + fromList [1, 2, 3] `shouldBe` fromList [2, 4]
    it "works for arbitrary polynomials" $
        forAll ((,) <$> polyGen <*> polyGen) $ \(p1, p2) ->
            p1 - p2 + p2 `shouldBe` p1

productSpec :: Spec
productSpec = do
    it "0 * 0 == 0" $
        zeroPoly * zeroPoly `shouldBe` zeroPoly
    it "0 * poly == 0 for any poly" $
        forAll polyGen $ \poly -> zeroPoly * poly `shouldBe` zeroPoly
    it "poly * 0 == 0 for any poly" $
        forAll polyGen $ \poly -> poly * zeroPoly `shouldBe` zeroPoly
    it "works for small polynomials" $ do
        fromList @Integer [-1, 1] * fromList [1, 1, 1, 1]
            `shouldBe` fromList [1, 0, 0, 0, 1]
        fromList @Integer [1, 2, 3] * fromList [4, 5, 6]
            `shouldBe` fromList [4, 13, 28, 27, 18]
    it "poly * 1 == poly for any poly" $
        forAll polyGen $ \poly -> poly * fromList [1] `shouldBe` poly
    it "1 * poly == poly for any poly" $
        forAll polyGen $ \poly -> fromList [1] * poly `shouldBe` poly
    it "nonzero polys p, q, satisfy degree(p*q) == degree(p) + degree(q)" $ do
        let nonzeroPolyGen = polyGen `suchThat` (/= zeroPoly)
        forAll ((,) <$> nonzeroPolyGen <*> nonzeroPolyGen) $ \(p, q) ->
            degree (p * q) `shouldBe` degree p + degree q

absSpec :: Spec
absSpec = do
    it "abs of zero polynomial is zero polynomial" $
        abs zeroPoly `shouldBe` zeroPoly
    it "abs of positive polynomial is the same polynomial" $ do
        abs (fromList [3]) `shouldBe` fromList [3]
        abs (fromList [-1, -2, 3]) `shouldBe` fromList [-1, -2, 3]
    it "abs of positive polynomial is the same polynomial" $ do
        abs (fromList [1, 2, -3]) `shouldBe` fromList [-1, -2, 3]
        abs (fromList [-3]) `shouldBe` fromList [3]

signumSpec :: Spec
signumSpec = do
    it "signum of polynomials with positive leading coefficient is 1" $ do
        signum (fromList [-3, 0, 4]) `shouldBe` 1
        signum (fromList [-3, 0, 4, 0]) `shouldBe` 1
        signum (fromList [3]) `shouldBe` 1
    it "signum of polynomials with negative leading coefficient is -1" $ do
        signum (fromList [3, 0, -4]) `shouldBe` (-1)
        signum (fromList [3, 0, -4, 0]) `shouldBe` (-1)
        signum (fromList [-3]) `shouldBe` (-1)
    it "signum of zero polynomial is 0" $ do
        signum zeroPoly `shouldBe` 0
        signum (fromList [0]) `shouldBe` 0

absSignumInvariantSpec :: Spec
absSignumInvariantSpec = do
    it "abs poly * signum poly == poly for zero polynomial" $
        abs zeroPoly * signum zeroPoly `shouldBe` zeroPoly
    it "abs poly * signum poly == poly for any polynomial" $
        forAll polyGen $ \poly -> abs poly * signum poly `shouldBe` poly

fromIntegerSpec :: Spec
fromIntegerSpec = do
    it "works for zero" $
        fromInteger (0 :: Integer) `shouldBe` zeroPoly
    it "works for arbitrary integers" $
        forAll (choose (-maxCoefficient, maxCoefficient) `suchThat` (/= 0)) $
            \n -> fromInteger n `shouldBe` fromList [n]

negateSpec :: Spec
negateSpec = do
    it "negating zero polynomial gives zero" $ do
        negate zeroPoly `shouldBe` zeroPoly
        negate (fromList [0]) `shouldBe` zeroPoly
        negate (fromList [0, 0]) `shouldBe` zeroPoly
    it "works for small polynomials" $ do
        negate (fromList @Integer [1, 2, 3]) `shouldBe` fromList [-1, -2, -3]
        negate (fromList @Integer [0, -1, 2, -3, 0]) `shouldBe` fromList [0, 1, -2, 3]
