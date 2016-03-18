module Isotope.Element
    ( elementMostAbundantIsotope
    , elementMonoisotopicMass
    , elementNominalMass
    , elementIsotopicMasses
    , elementIntegerMasses
    , elementAverageAtomicMass
    , elementIsotopicAbundances
    , massNumber
    ) where

import Isotope.Periodic
import Data.List           (elemIndex)
import Data.Maybe          (fromJust)

-- |Returns the most abundant naturally-occurring isotope for an eleElementment.
elementMostAbundantIsotope :: Element -> Isotope
elementMostAbundantIsotope e = isotopeList !! indexOfIsotope
    where isotopeList = isotopes' e
          abundances = isotopicAbundance <$> isotopeList
          indexOfIsotope = fromJust $ elemIndex (maximum abundances) abundances

-- |Monoisotopic mass for an element. Monoisotopic mass is the exact mass of the
-- most abundant isotope.
elementMonoisotopicMass :: Element -> MonoisotopicMass
elementMonoisotopicMass = isotopicMass . elementMostAbundantIsotope

-- |Nominal mass for an element. Nominal mass is the integer mass of the
-- most abundant isotope.
elementNominalMass :: Element -> NominalMass
elementNominalMass = massNumber . nucleons . elementMostAbundantIsotope

-- |Exact masses for all naturally-occurring isotopes for an element.
elementIsotopicMasses :: Element -> [IsotopicMass]
elementIsotopicMasses e = isotopicMass <$> isotopes' e

-- |Integer masses for all naturally-occurring isotopes for an element.
elementIntegerMasses :: Element -> [IntegerMass]
elementIntegerMasses e = massNumber . nucleons <$> isotopes' e

-- |Average atomic mass for an element.
elementAverageAtomicMass :: Element -> AverageAtomicMass
elementAverageAtomicMass e = sum [isotopicMass x * isotopicAbundance x |
                             x <- isotopes' e]

-- |Isotope abundances for all naturally-occurring isotopes for an element.
elementIsotopicAbundances :: Element -> [IsotopicAbundance]
elementIsotopicAbundances e = isotopicAbundance <$> isotopes' e

-- |Mass number for an isotope. Mass number is the number of protons plus the
-- number of neutrons.
massNumber :: (ProtonNumber, NeutronNumber) -> MassNumber
massNumber (protonNum, neutronNum) = protonNum + neutronNum
