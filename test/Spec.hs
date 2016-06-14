module Main where

import Test.Hspec
import Test.QuickCheck
import Isotope.BaseSpec
import Isotope.ParsersSpec

main :: IO ()
main = hspec $ do
  describe "Base" Isotope.BaseSpec.spec
  describe "Parsers" Isotope.ParsersSpec.spec
