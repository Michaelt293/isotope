{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
module Isotope.BaseSpec (spec) where

import Isotope
import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.Char
import Data.List

spec :: Spec
spec = do
    describe "ElementSymbol" $ do
      it "should not have more than two characters" $
        ((<= 2) . length . show) <$> elementSymbolList `shouldSatisfy` and
      it "second character of an ElementSymbol should not be upper case" $
        (\x -> length x /= 2 || (isLower . last) x) . show <$>
        elementSymbolList `shouldSatisfy` and

    describe "elementSymbolList" .
      it "should not contain duplicate elements" $
        findElement <$> elementSymbolList `shouldSatisfy` allUnique

    describe "elementName" $ do
      it "should not be an empty string" $
        elementName <$> elementSymbolList `shouldSatisfy` notElem ""
      it "should not start with a capital letter" $
        isLower . head . elementName <$> elementSymbolList `shouldSatisfy` and
      it "element names should not be duplicated" $
        elementName <$> elementSymbolList `shouldSatisfy` allUnique

    describe "atomicNumber" $ do
      it "should be between 1 and 92" $
        (\x -> x >= 1 && x <= 92) . atomicNumber <$>
        elementSymbolList `shouldSatisfy` and
      it "should not have duplicated atomic numbers" $
        atomicNumber <$> elementSymbolList `shouldSatisfy` allUnique

    describe "isotopes" .
      it "should not have duplicate isotopes" $
        isotopes <$> elementSymbolList `shouldSatisfy` allUnique

    describe "mostAbundantIsotope" .
      it "C should be C12 (six protons and six neutrons)" $
        (nucleons . mostAbundantIsotope) C `shouldBe` (6, 6)

    describe "selectIsotope" .
      it "calling fuction with the arguments C and 12 should select C12" $
        nucleons <$> selectIsotope C 12 `shouldBe` Just (6, 6)

    describe "monoisotopicMass" .
      it "calling function with C should be 12.0" $
        monoisotopicMass C `shouldBe` MonoisotopicMass 12.0

    describe "nominalMass" .
      it "calling function with C should return 12" $
        nominalMass C `shouldBe` NominalMass 12

    describe "isotopicMasses" .
      it "H should return a list containing 1.007... and 2.014...." $
        isotopicMasses H `shouldBe` IsotopicMass <$> [1.00782503223, 2.01410177812]

    describe "integerMasses" $ do
      it "calling function with H should return [1, 2]" $
         integerMasses H `shouldBe` [1, 2]
      it "proton number should be equal to atomic number" $
        all protonNumEqAtomicNum elementSymbolList

    describe "averageMass" .
      it "calling function with C should return 12.0107" $
        withinTolerance (getAverageMass (averageMass C)) 12.0107 0.0001 `shouldBe` True

    describe "isotopicAbundances" $ do
      it "calling function with C should return [0.9893, 0.0107]" $
        isotopicAbundances C `shouldBe` IsotopicAbundance <$> [0.9893, 0.0107]
      it "sum of isotopic abundances for an element should equal 1" $
        (\sym -> withinTolerance (sum (getIsotopicAbundance <$> isotopicAbundances sym)) 1 0.0001) <$>
        elementSymbolList `shouldSatisfy` and

    describe "renderFormula for ElementalComposition" $ do
      it "[ele|C6H6O|] should be \"C6H6O\"" $
        renderFormula [ele|C6H6O|] `shouldBe` "C6H6O"
      it "[ele|CCl4|] should be \"CCl4\"" $
        renderFormula [ele|CCl4|] `shouldBe` "CCl4"
      it "[ele|H2O4S|] should be \"H2O4S\"" $
        renderFormula [ele|H2O4S|] `shouldBe` "H2O4S"

    describe "renderFormula for MolecularFormula" $ do
      it "[mol|C6H6O|] should be \"C6H6O\"" $
        renderFormula [mol|C6H6O|] `shouldBe` "C6H6O"
      it "[mol|CCl4|] should be \"CCl4\"" $
        renderFormula [mol|CCl4|] `shouldBe` "CCl4"
      it "[mol|H2O4S|] should be \"H2O4S\"" $
        renderFormula [mol|H2O4S|] `shouldBe` "H2O4S"
      it "[mol|BBr3|] should be \"BBr3\"" $
        renderFormula [mol|BBr3|] `shouldBe` "BBr3"

    describe "renderFormula for CondensedFormula" $ do
      it "[con|C6H6O|] should be \"C6H6O\"" $
        renderFormula [con|C6H6O|] `shouldBe` "C6H6O"
      it "[con|N(CH3)3|] should be \"N(CH3)3\"" $
        renderFormula [con|N(CH3)3|] `shouldBe` "N(CH3)3"
      it "[con|C6H5OH|] should be \"C6H5OH\"" $
        renderFormula [con|C6H5OH|] `shouldBe` "C6H5OH"

    describe "renderFormula for EmpiricalFormula" $ do
      it "[emp|C6H6O|] should be \"C6H6O\"" $
        renderFormula [emp|C6H6O|] `shouldBe` "C6H6O"
      it "[emp|CCl4|] should be \"CCl4\"" $
        renderFormula [emp|CCl4|] `shouldBe` "CCl4"
      it "[emp|H2O4S|] should be \"H2O4S\"" $
        renderFormula [emp|H2O4S|] `shouldBe` "H2O4S"

    describe "ToElementalComposition - ElementalComposition instance" $ do
      it "toElementalComposition" . property $
        \ec -> toElementalComposition ec == (ec :: ElementalComposition)
      it "charge of an elemental composition should Just 0" . property $
        \ec -> charge (ec :: ElementalComposition) == Nothing
      it "monoisotopic mass of ethanol" $
        withinTolerance (getMonoisotopicMass (monoisotopicMass [ele|C2H6O|])) 46.04186 0.0001
        `shouldBe` True
      it "average mass of ethanol" $
        withinTolerance (getAverageMass (averageMass [ele|C2H6O|])) 46.06844 0.0001
        `shouldBe` True
      it "nominalMass mass of ethanol" $
        nominalMass [ele|C2H6O|] `shouldBe` NominalMass 46

    describe "mkElementalComposition" $ do
      it "zero is filtered out" $
        mkElementalComposition [(C, 0), (H, 0), (O, 0)] `shouldBe` emptyFormula
      it "should give the correct formula" $
        mkElementalComposition [(C, 2), (H, 6), (O, 1)] `shouldBe` [ele|C2H6O|]

    describe "ToElementalComposition - ElementSymbol instance" $ do
      it "monoisotopicMass" . property $
        \sym -> monoisotopicMass sym == monoisotopicMass (mkElementalComposition [(sym, 1)])
      it "charge of a symbol should be Just 0" . property $
        \sym -> charge (sym :: ElementSymbol) == Nothing

    describe "Monoid instance for MolecularFormula" $ do
      it "associativity" . property $
          \a b c -> (a |+| b) |+| c == a |+| (b |+| c :: MolecularFormula)
      it "right identity" . property $
          \a -> (a :: MolecularFormula) |+| emptyFormula == a
      it "left identity" . property $
          \a -> (emptyFormula :: MolecularFormula) |+| a == a

    describe "Addition of molecular formulae is commutative" .
      it "commutative" . property $
        \a b -> (a |+| b) == (b |+| a :: MolecularFormula)

    describe "properties of |+|, |*| and |-|)" $ do
      it "a |-| a = emptyFormula" . property $
        \a -> a |-| a == (emptyFormula :: MolecularFormula)
      it "0 |*| a == emptyFormula" . property $
        \a -> a |*| 0 == (emptyFormula :: MolecularFormula)
      it "a |+| a == 2 |*| a" . property $
        \a -> a |+| a == (a :: MolecularFormula) |*| 2

    describe "ToElementalComposition - MolecularFormula instance" $ do
      it "[mol|C2H6O|] should be [ele|C2H6O|]" $
        toElementalComposition [mol|C2H6O|] `shouldBe` [ele|C2H6O|]
      it "empty MolecularFormula should return an empty ElementalComposition" $
        toElementalComposition (emptyFormula :: ElementalComposition) `shouldBe`
        mkElementalComposition []

    describe "Monoid instance for CondensedFormula" $ do
      it "associativity" . property $
          \a b c -> (a `mappend` b) `mappend` c ==
                     a `mappend` (b `mappend` c :: CondensedFormula)
      it "right identity" . property $
          \a -> (a :: CondensedFormula) `mappend` mempty == a
      it "left identity" . property $
          \a -> emptyFormula `mappend` (a :: CondensedFormula) == a

    describe "ToElementalComposition - CondensedFormula instance" $ do
      it "empty CondensedFormula should return an empty ElementalComposition" $
        toElementalComposition (emptyFormula :: CondensedFormula) `shouldBe`
        [ele||]
      it "[con|N(CH3)3|] should be [ele|C3NH9|]" $
        toElementalComposition [con|N(CH3)3|] `shouldBe` [ele|C3NH9|]

    describe "ToMolecularFormula - CondensedFormula instance" $ do
      it "empty CondensedFormula should return an empty MolecularFormula" $
        toMolecularFormula (emptyFormula :: CondensedFormula) `shouldBe`
        mkMolecularFormula []
      it "[mol|C6H6|] should be [emp|CH|]" $
        toMolecularFormula [con|N(CH3)3|] `shouldBe` [mol|C3NH9|]

    describe "mkEmpiricalFormula" $ do
      it "zero is filtered out" $
        mkEmpiricalFormula [(C, 0), (H, 0), (O, 0)] `shouldBe` emptyFormula
      it "should give the correct formula" $
        mkEmpiricalFormula [(C, 6), (H, 6)] `shouldBe` [emp|CH|]

    describe "ToEmpiricalFormula - ElementalComposition instance" $ do
      it "empty ElementalComposition should return an empty EmpiricalFormula" $
        toEmpiricalFormula (emptyFormula :: ElementalComposition) `shouldBe`
        mkEmpiricalFormula []
      it "[ele|C6H6|] should be [emp|CH|]" $
        toEmpiricalFormula [ele|C6H6|] `shouldBe` [emp|CH|]

    describe "ToEmpiricalFormula - MolecularFormula instance" $ do
      it "empty MolecularFormula should return an empty EmpiricalFormula" $
        toEmpiricalFormula (emptyFormula :: MolecularFormula) `shouldBe`
        mkEmpiricalFormula []
      it "[mol|C6H6|] should be [emp|CH|]" $
        toEmpiricalFormula [mol|C6H6|] `shouldBe` [emp|CH|]

    describe "ToEmpiricalFormula - CondensedFormula instance" $ do
      it "empty CondensedFormula should return an empty EmpiricalFormula" $
        toEmpiricalFormula (emptyFormula :: CondensedFormula) `shouldBe`
        mkEmpiricalFormula []
      it "[con|C6H6|] should be [emp|CH|]" $
        toEmpiricalFormula [con|C6H6|] `shouldBe` [emp|CH|]

    describe "ToElementalComposition - EmpiricalFormula instance" $ do
      it "empty EmpiricalFormula should return an empty EmpiricalFormula" $
        toElementalComposition (emptyFormula :: EmpiricalFormula) `shouldBe`
        mkElementalComposition []
      it "[emp|CH|] should be [ele|CH|]" $
        toElementalComposition [emp|CH|] `shouldBe` [ele|CH|]

    describe "Laws for ElementalComposition, MolecularFormula, EmpiricalFormula and CondensedFormula data types" $ do
      it "applying toEmpiricalFormula to a CondensedFormula should give the same result as applying toMolecularFormula compose toEmpiricalFormula" . property $
        \c -> toEmpiricalFormula c == (toEmpiricalFormula . toMolecularFormula) (c :: CondensedFormula)
      it "applying toElementalComposition to a CondensedFormula should give the same result as applying toMolecularFormula compose toElementalComposition" . property $
        \c -> toElementalComposition c == (toElementalComposition . toMolecularFormula) (c :: CondensedFormula)
      it "applying toElementalComposition compose toEmpiricalFormula to an EmpiricalFormula should return the same EmpiricalFormula" . property $
        \e -> (toEmpiricalFormula . toElementalComposition) e == (e :: EmpiricalFormula)

allUnique :: (Eq a) => [a] -> Bool
allUnique l = l == nub l

withinTolerance :: (Num a, Ord a) => a -> a -> a -> Bool
withinTolerance n1 n2 err = abs (n1 - n2) < err

protonNumEqAtomicNum :: ElementSymbol -> Bool
protonNumEqAtomicNum sym =
  and $ (== atomicNumber sym) . fst . nucleons <$> isotopes sym

elemSymIntPairGen :: Gen (ElementSymbol, Int)
elemSymIntPairGen = do
    elemSym <- arbitrary
    n <- choose (1,100)
    return (elemSym, n)

elemSymIntPairListGen :: Gen [(ElementSymbol, Int)]
elemSymIntPairListGen = listOf elemSymIntPairGen

instance Arbitrary ElementSymbol where
    arbitrary = oneof $ return <$> elementSymbolList

instance Arbitrary ElementalComposition where
  arbitrary = mkElementalComposition <$> elemSymIntPairListGen

instance Arbitrary MolecularFormula where
  arbitrary = mkMolecularFormula <$> elemSymIntPairListGen

instance Arbitrary CondensedFormula where
  arbitrary = do
    n <- choose (0, 4)
    condForm <- vectorOf n
      (oneof [leftCondensedFormulaGen, sized arbRightCondensedFormulaGen])
    return $ CondensedFormula condForm
    where
      leftCondensedFormulaGen :: Gen (Either MolecularFormula (CondensedFormula, Int))
      leftCondensedFormulaGen = Left <$> arbitrary
      arbRightCondensedFormulaGen :: Int -> Gen (Either MolecularFormula (CondensedFormula, Int))
      arbRightCondensedFormulaGen 0 = do
        m <- choose (1, 4)
        condForm <- leftCondensedFormulaGen
        return $ Right (CondensedFormula [condForm], m)
      arbRightCondensedFormulaGen n | n > 0 = do
        m <- choose (1, 4)
        v <- choose (1, 3)
        let n' = n `div` (v + 1)
        condForm' <- replicateM v (arbRightCondensedFormulaGen n')
        return $ Right (CondensedFormula condForm', m)

instance Arbitrary EmpiricalFormula where
  arbitrary = mkEmpiricalFormula <$> elemSymIntPairListGen
