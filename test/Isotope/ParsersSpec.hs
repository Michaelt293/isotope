{-# LANGUAGE QuasiQuotes #-}
module Isotope.ParsersSpec (spec) where

import Isotope
import Test.Hspec
import Test.QuickCheck
import Data.Map (fromList)

spec :: Spec
spec = do
    describe "mol" $
      it "QuasiQuoter for molecular formulae should produce the correct value" $
        [mol|C6H6|] `shouldBe` MolecularFormula {getMolecularFormula = fromList [(H,6),(C,6)]}
    describe "emp" $
      it "QuasiQuoter for empirical formulae should produce the correct value" $
        [emp|CH|] `shouldBe` EmpiricalFormula {getEmpiricalFormula = fromList [(H,1),(C,1)]}
    describe "con" $
      it "QuasiQuoter for condensed formulae should produce the correct value" $
        [con|CH3(CH3)2CH3|] `shouldBe` CondensedFormula {getCondensedFormula = [Left (MolecularFormula {getMolecularFormula = fromList [(C,1)]}),Left (MolecularFormula {getMolecularFormula = fromList [(H,3)]}),Right ([MolecularFormula {getMolecularFormula = fromList [(C,1)]},MolecularFormula {getMolecularFormula = fromList [(H,3)]}],2),Left (MolecularFormula {getMolecularFormula = fromList [(C,1)]}),Left (MolecularFormula {getMolecularFormula = fromList [(H,3)]})]}
