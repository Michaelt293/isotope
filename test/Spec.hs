{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Isotope
import Test.Hspec
import Test.QuickCheck
import Data.Char
import Data.List
import Data.Map (Map, fromList)

main :: IO ()
main = hspec $ do
  describe "Elements" $ do

    describe "ElementSymbol" $ do
      it "should not have more than two characters" $
        ((<= 2) . length . show) <$> elementSymbolList `shouldSatisfy` and
      it "second character of a two character ElementSymbol should not be upper case" $
        (\x -> length x /= 2 || (isLower . last) x) . show <$> elementSymbolList `shouldSatisfy` and
    describe "lookupElement" $ do
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

    describe "isotopes" $ do
      it "should not have duplicate isotopes" $
        isotopes <$> elementSymbolList `shouldSatisfy` allUnique

    describe "mostAbundantIsotope" $ do
      it "C should be C12 (six protons and six neutrons)" $
        (nucleons . mostAbundantIsotope) C `shouldBe` (6, 6)

    describe "selectIsotope" $ do
      it "calling fuction with the arguments C and 12 should select C12" $
        nucleons (selectIsotope C 12) `shouldBe` (6, 6)

    describe "monoisotopicMass" $ do
      it "calling function with C should be 12.0" $
        monoisotopicMass C `shouldBe` 12.0

    describe "nominalMass" $ do
      it "calling function with C should return 12" $
        nominalMass C `shouldBe` 12

    describe "isotopicMasses" $ do
      it "calling function with H should return a list containing 1.007... and 2.014...." $
        isotopicMasses H `shouldBe` [1.00782503223, 2.01410177812]

    describe "integerMasses" $ do
      it "calling function with H should return [1, 2]" $
         integerMasses H `shouldBe` [1, 2]
      it "proton number should be equal to atomic number" $
        all protonNumEqAtomicNum elementSymbolList

    describe "averageAtomicMass" $ do
      it "calling function with C should return 12.0107" $
        withinTolerance (averageMass C) 12.0107 0.0001 `shouldBe` True

    describe "isotopicAbundances" $ do
      it "calling function with C should return [0.9893, 0.0107]" $
        isotopicAbundances C `shouldBe` [0.9893, 0.0107]
      it "sum of isotopic abundances for an element should equal 0 or 1" $
        (\sym -> sumIsotopicAbundance sym == 0
                     || withinTolerance (sumIsotopicAbundance sym) 1 0.0001) <$> elementSymbolList `shouldSatisfy` and

    describe "Monoid instance for MolecularFormula" $ do
      it "associativity" $ property $
          \a b c -> (a |+| b) |+| c == a |+| (b |+| c)
      it "identity element" $ property $
          \a -> a |+| emptyFormula == a

    describe "Addition of molecular formulae is commutative" $ do
      it "commutative" $ property $
        \a b -> a |+| b == b |+| a

    describe "properties of |+|, |*| and |-|)" $ do
      it "a |-| a = emptyFormula" $ property $
        \a -> a |-| a == emptyFormula
      it "a |+| -a == emptyFormula" $ property $
        \a -> a |+| MolecularFormula (negate <$> getMolecularFormula a) == emptyFormula
      it "0 |*| a == emptyFormula" $ property $
        \a -> 0 |*| a == emptyFormula
      it "a |+| a == 2 |*| a" $ property $
        \a -> a |+| a == 2 |*| a

    describe "ToEmpiricalFormula" $ do
      it "Empty MolecularFormula should return an empty EmpiricalFormula" $
        toEmpiricalFormula (emptyFormula :: MolecularFormula) `shouldBe` mkEmpiricalFormula []
      it "\"C6H6\" should be \"CH\"" $
        toEmpiricalFormula ("C6H6" :: MolecularFormula) `shouldBe` ("CH" :: EmpiricalFormula)
      it "\"(CH)6\" should be \"CH\"" $
        toEmpiricalFormula ("(CH)6" :: CondensedFormula) `shouldBe` ("CH" :: EmpiricalFormula)

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

instance Arbitrary (ElementSymbol, Int) where
    arbitrary = do
      elemSym <- arbitrary :: Gen ElementSymbol
      n <- choose (1,100)
      return (elemSym, n)

instance Arbitrary (Map ElementSymbol Int) where
    arbitrary = do
      symNums <- arbitrary :: Gen [(ElementSymbol, Int)]
      return $ fromList symNums

instance Arbitrary MolecularFormula where
    arbitrary = do
      symNums <- arbitrary :: Gen (Map ElementSymbol Int)
      return $ MolecularFormula symNums
