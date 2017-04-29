module Main where

import Test.Hspec
import Test.QuickCheck
import Isotope.BaseSpec
import Isotope.ParsersSpec
import Isotope.IonSpec

main :: IO ()
main = hspec $ do
  describe "Base" Isotope.BaseSpec.spec
  describe "Parsers" Isotope.ParsersSpec.spec
  describe "Ion" Isotope.IonSpec.spec
