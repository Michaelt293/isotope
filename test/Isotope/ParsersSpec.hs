{-# LANGUAGE QuasiQuotes #-}
module Isotope.ParsersSpec (spec) where

import Isotope
import Isotope.Parsers
import Test.Hspec
import Test.QuickCheck
import Data.Map (fromList)
import Text.Megaparsec

spec :: Spec
spec = do
  describe "elementSymbol" .
    it "Na" $
      parse (elementSymbol <* eof) "" "Na" `shouldBe` Right Na
  describe "subFormula" .
    it "C2" $
      parse (subFormula <* eof) "" "C2" `shouldBe` Right (C, 2)
  describe "molecularFormula" .
    it "CH2O" $
      parse (molecularFormula <* eof) "" "CH2O" `shouldBe` Right (MolecularFormula (fromList [(C, 1), (H, 2), (O, 1)]))
  describe "condensedFormula" .
    it "N(CH3)3" $
      parse (condensedFormula <* eof) "" "N(CH3)3" `shouldBe` Right (CondensedFormula {getCondensedFormula = [Left (MolecularFormula {getMolecularFormula = fromList [(N,1)]}),Right (CondensedFormula {getCondensedFormula = [Left (MolecularFormula {getMolecularFormula = fromList [(C,1)]}),Left (MolecularFormula {getMolecularFormula = fromList [(H,3)]})]},3)]})
  describe "mol" .
    it "QuasiQuoter for MolecularFormula should produce correct value" $
      [mol|C6H6|] `shouldBe`
      MolecularFormula {getMolecularFormula = fromList [(H,6),(C,6)]}
  describe "emp" .
    it "QuasiQuoter for EmpiricalFormula should produce correct value" $
      [emp|CH|] `shouldBe`
      EmpiricalFormula {getEmpiricalFormula = fromList [(H,1),(C,1)]}
  describe "ele" .
    it "QuasiQuoter for ElementalComposition should produce correct value" $
      [ele|C6H6|] `shouldBe`
      ElementalComposition {getElementalComposition = fromList [(H,6),(C,6)]}
  describe "con" .
    it "QuasiQuoter for CondensedFormula should produce the correct value" $
      [con|CH3(CH3)2CH3|] `shouldBe` CondensedFormula {getCondensedFormula = [Left (MolecularFormula {getMolecularFormula = fromList [(C,1)]}),Left (MolecularFormula {getMolecularFormula = fromList [(H,3)]}),Right (CondensedFormula {getCondensedFormula = [Left (MolecularFormula {getMolecularFormula = fromList [(C,1)]}),Left (MolecularFormula {getMolecularFormula = fromList [(H,3)]})]},2),Left (MolecularFormula {getMolecularFormula = fromList [(C,1)]}),Left (MolecularFormula {getMolecularFormula = fromList [(H,3)]})]}
