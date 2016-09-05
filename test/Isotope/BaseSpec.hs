{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Isotope.BaseSpec (spec) where

import Isotope
import Test.Hspec
import Test.QuickCheck
import Data.Char
import Data.List
import Data.Map (Map, fromList)

spec :: Spec
spec = do
    describe "ElementSymbol" $ do
      it "should not have more than two characters" $
        ((<= 2) . length . show) <$> elementSymbolList `shouldSatisfy` and
      it "second character of a two character ElementSymbol should not be upper case" $
        (\x -> length x /= 2 || (isLower . last) x) . show <$> elementSymbolList `shouldSatisfy` and
    describe "lookupElement" $
      it "should not contain duplicate elements" $
        lookupElement <$> elementSymbolList `shouldSatisfy` allUnique
    describe "elementName" $ do
      it "should not be an empty string" $
        elementName <$> elementSymbolList `shouldSatisfy` notElem ""
      it "should not start with a capital letter" $
        isLower . head . elementName <$> elementSymbolList `shouldSatisfy` and
      it "element names should not be duplicated" $
        elementName <$> elementSymbolList `shouldSatisfy` allUnique

    describe "atomicNumber" $ do
      it "should be between 1 and 92" $
        (\x -> x >= 1 && x <= 92) . atomicNumber <$> elementSymbolList `shouldSatisfy` and
      it "should not have duplicated atomic numbers" $
        atomicNumber <$> elementSymbolList `shouldSatisfy` allUnique

    describe "isotopes" $
      it "should not have duplicate isotopes" $
        isotopes <$> elementSymbolList `shouldSatisfy` allUnique

    describe "mostAbundantIsotope" $
      it "C should be C12 (six protons and six neutrons)" $
        (nucleons . mostAbundantIsotope) C `shouldBe` (6, 6)

    describe "selectIsotope" $
      it "calling fuction with the arguments C and 12 should select C12" $
        nucleons (selectIsotope C 12) `shouldBe` (6, 6)

    describe "monoisotopicMass" $
      it "calling function with C should be 12.0" $
        monoisotopicMass C `shouldBe` 12.0

    describe "nominalMass" $
      it "calling function with C should return 12" $
        nominalMass C `shouldBe` 12

    describe "isotopicMasses" $
      it "calling function with H should return a list containing 1.007... and 2.014...." $
        isotopicMasses H `shouldBe` [1.00782503223, 2.01410177812]

    describe "integerMasses" $ do
      it "calling function with H should return [1, 2]" $
         integerMasses H `shouldBe` [1, 2]
      it "proton number should be equal to atomic number" $
        all protonNumEqAtomicNum elementSymbolList

    describe "averageAtomicMass" $
      it "calling function with C should return 12.0107" $
        withinTolerance (averageMass C) 12.0107 0.0001 `shouldBe` True

    describe "isotopicAbundances" $ do
      it "calling function with C should return [0.9893, 0.0107]" $
        isotopicAbundances C `shouldBe` [0.9893, 0.0107]
      it "sum of isotopic abundances for an element should equal 1" $
        (\sym -> withinTolerance (sumIsotopicAbundance sym) 1 0.0001) <$> elementSymbolList `shouldSatisfy` and

    describe "Monoid instance for MolecularFormula" $ do
      it "associativity" $ property $
          \a b c -> (a |+| b) |+| c == a |+| (b |+| c)
      it "identity element" $ property $
          \a -> a |+| emptyFormula == a

    describe "Addition of molecular formulae is commutative" $
      it "commutative" $ property $
        \a b -> a |+| b == b |+| a

    describe "properties of |+|, |*| and |-|)" $ do
      it "a |-| a = emptyFormula" $ property $
        \a -> a |-| a == emptyFormula
      it "0 |*| a == emptyFormula" $ property $
        \a -> 0 |*| a == emptyFormula
      it "a |+| a == 2 |*| a" $ property $
        \a -> a |+| a == 2 |*| a

    describe "toEmpiricalFormula" $ do
      it "Empty MolecularFormula should return an empty EmpiricalFormula" $
        toEmpiricalFormula (emptyFormula :: MolecularFormula) `shouldBe` mkEmpiricalFormula []
      it "\"C6H6\" should be \"CH\"" $
        toEmpiricalFormula [mol|C6H6|] `shouldBe` [emp|CH|]

    describe "renderFormula" $ do
      it "\"C6H6O\" should be \"C6H6O\"" $
        renderFormula [mol|C6H6O|] `shouldBe` "C6H6O"
      it "\"CCl4\" should be \"CCl4\"" $
        renderFormula [mol|CCl4|] `shouldBe` "CCl4"
      it "\"H2O4S\" should be \"H2O4S\"" $
        renderFormula [mol|H2O4S|] `shouldBe` "H2O4S"


allUnique :: (Eq a) => [a] -> Bool
allUnique l = l == nub l

withinTolerance :: (Num a, Ord a) => a -> a -> a -> Bool
withinTolerance n1 n2 err = abs (n1 - n2) < err

protonNumEqAtomicNum :: ElementSymbol -> Bool
protonNumEqAtomicNum sym = and $ (== atomicNumber sym) . fst . nucleons <$> isotopes sym

sumIsotopicAbundance :: ElementSymbol -> IsotopicAbundance
sumIsotopicAbundance = sum . isotopicAbundances

instance Arbitrary ElementSymbol where
    arbitrary = oneof $ return <$> elementSymbolList

elemSymIntPairGen :: Gen (ElementSymbol, Int)
elemSymIntPairGen = do
    elemSym <- arbitrary
    n <- choose (1,100)
    return (elemSym, n)

elemSymIntPairListGen :: Gen [(ElementSymbol, Int)]
elemSymIntPairListGen = listOf elemSymIntPairGen

instance Arbitrary MolecularFormula where
    arbitrary = do
      xs <- elemSymIntPairListGen
      return $ MolecularFormula . fromList $ xs
