{-|
Module      : ElementIsotopes
Description : Isotopic masses and relative abundances for all elements from
              Hydrogen to Bismuth and Thorium and Uranium. Data on isotopic
              masses and relative abundances obtained from-
              http://physics.nist.gov/cgi-bin/Compositions/stand_alone.pl/
              (Accessed: 28/8/2015; uncertainities not provided).
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental
-}
module ElementIsotopes
    ( elementSymbolList
    , lookupElement
    , elementName
    , atomicNumber
    , isotopes
    , mostAbundantIsotope
    , selectIsotope
    , monoisotopicMass
    , nominalMass
    , integerMasses
    , isotopicMasses
    , averageAtomicMass
    , isotopicAbundances
    ) where

import Prelude hiding      (lookup)
import qualified Data.Map as Map
import Data.List           (elemIndex)
import Data.Maybe          (fromJust)
import Periodic
import Element

lookup :: ElementSymbol -> ElementSymbolMap a -> Maybe a
lookup k m = Map.lookup k (getSymbolMap m)

(!) :: ElementSymbolMap a -> ElementSymbol -> a
m ! k = getSymbolMap m Map.! k

-- |Searches elements (a map) with an ElementSymbol key and returns
-- information on for the element.
lookupElement :: ElementSymbol -> Maybe Element
lookupElement = flip lookup elements

findElement :: ElementSymbol -> Element
findElement = (!) elements

-- |Returns the name for an element symbol.
elementName :: ElementSymbol -> ElementName
elementName = elementName' . findElement

-- |Returns the atomic number for an element.
atomicNumber :: ElementSymbol -> AtomicNumber
atomicNumber = atomicNumber' . findElement

-- |Returns all the naturally-occurring isotopes for an element.
isotopes :: ElementSymbol -> [Isotope]
isotopes = isotopes' . findElement

-- |Returns the most abundant naturally-occurring isotope for an element.
mostAbundantIsotope :: ElementSymbol -> Isotope
mostAbundantIsotope = elementMostAbundantIsotope . findElement

-- |Selects an isotope of element based on the isotope's mass number (IntegerMass).
selectIsotope :: ElementSymbol -> IntegerMass -> Isotope
selectIsotope sym mass = isotopeList !! indexOfIsotope
    where isotopeList = isotopes sym
          indexOfIsotope = fromJust $ elemIndex mass (integerMasses sym)

-- |Monoisotopic mass for an element. Monoisotopic mass is the exact mass of the
-- most abundant isotope.
monoisotopicMass :: ElementSymbol -> MonoisotopicMass
monoisotopicMass = elementMonoisotopicMass . findElement

-- |Nominal mass for an element. Nominal mass is the integer mass of the
-- most abundant isotope.
nominalMass :: ElementSymbol -> NominalMass
nominalMass = elementNominalMass . findElement

-- |Exact masses for all naturally-occurring isotopes for an element.
isotopicMasses :: ElementSymbol -> [IsotopicMass]
isotopicMasses = elementIsotopicMasses . findElement

-- |Integer masses for all naturally-occurring isotopes for an element.
integerMasses :: ElementSymbol -> [IntegerMass]
integerMasses = elementIntegerMasses . findElement

-- |Average atomic mass for an element.
averageAtomicMass :: ElementSymbol -> AverageAtomicMass
averageAtomicMass = elementAverageAtomicMass . findElement

-- |Isotope abundances for all naturally-occurring isotopes for an element.
isotopicAbundances :: ElementSymbol -> [IsotopicAbundance]
isotopicAbundances = elementIsotopicAbundances . findElement
