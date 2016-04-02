{-|
Module      : Isotopic.Element
Description : Contains functions which accept an `Element` as an argument.
              Import this module if required.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental

`ElementSymbolMap` is a functor. Consequently, it is possible to map the
functions defined in this module over 'elements' (type @ElementSymbolMap
Element@)
-}
module Isotope.Element (
    -- * Functions accepting an `Element` as the argument
      elementMostAbundantIsotope
    , elementIsotopicMasses
    , elementIntegerMasses
    , elementIsotopicAbundances
    ) where
import Isotope.Base
import Data.Maybe          (fromJust)
import Data.List           (elemIndex)

-- | Returns the most abundant naturally-occurring isotope for an element.
elementMostAbundantIsotope :: Element -> Isotope
elementMostAbundantIsotope e = isotopeList !! indexOfIsotope
    where isotopeList = isotopes' e
          abundances = isotopicAbundance <$> isotopeList
          indexOfIsotope = fromJust $ elemIndex (maximum abundances) abundances

-- | Exact masses for all naturally-occurring isotopes for an element.
elementIsotopicMasses :: Element -> [IsotopicMass]
elementIsotopicMasses e = isotopicMass <$> isotopes' e

-- | Integer masses for all naturally-occurring isotopes for an element.
elementIntegerMasses :: Element -> [IntegerMass]
elementIntegerMasses e = massNumber . nucleons <$> isotopes' e

-- | Isotope abundances for all naturally-occurring isotopes for an element.
elementIsotopicAbundances :: Element -> [IsotopicAbundance]
elementIsotopicAbundances e = isotopicAbundance <$> isotopes' e

-- Mass number for an isotope. Mass number is the number of protons plus the
-- number of neutrons.
massNumber :: Nucleons -> MassNumber
massNumber (protonNum, neutronNum) = protonNum + neutronNum

instance Mass Element where
   monoisotopicMass = isotopicMass . elementMostAbundantIsotope
   nominalMass = massNumber . nucleons . elementMostAbundantIsotope
   averageMass e = sum [isotopicMass x * isotopicAbundance x |
                                x <- isotopes' e]
