module Isotope.IonSpec (spec) where

import Isotope
import Isotope.Ion
import Test.Hspec

spec :: Spec
spec = do
  describe "mz" $ do
    it "The mass-to-charge ratio of protonated water should be 19.01838971626" $
      mz (Protonated water) `shouldBe` Mz {getMz = 19.01838971626}
    it "The mass-to-charge ratio of deprotonated water should be 17.0027396518" $
      mz (Deprotonated water) `shouldBe` Mz {getMz = 17.0027396518}

  describe "polarity" $ do
    it "The polarity of protonated water should be Positive" $
      polarity (Protonated water) `shouldBe` Positive
    it "The polarity of deprotonated water should be Negative" $
      polarity (Deprotonated water) `shouldBe` Negative

water :: MolecularFormula
water = mkMolecularFormula [(H, 2), (O, 1)]
