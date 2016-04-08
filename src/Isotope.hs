{-|
Module      : Isotope
Description : Isotope is a chemistry library for calculating masses of elements
              and molecules.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental

-}
module Isotope (
  -- * Type synonyms for masses
    IntegerMass
  , MonoisotopicMass
  , NominalMass
  , AverageMass
  , IsotopicMass
  -- * Other type synonyms
  , ElementName
  , IsotopicAbundance
  , AtomicNumber
  , ProtonNumber
  , NeutronNumber
  , Nucleons
  , MassNumber
  -- * 'Isotope' and 'Element' data types
  , Isotope
  , Element
  -- * Element symbols
  , ElementSymbol
  , elementSymbolList
  -- * 'ElementSymbolMap'
  , ElementSymbolMap
  , mkElementSymbolMap
  , lookup
  , (!)
  -- * Functions taking an 'Element' as input
  , elementMostAbundantIsotope
  , elementIsotopicMasses
  , elementIntegerMasses
  , elementIsotopicAbundances
  , elementMonoisotopicMass
  , elementNominalMass
  , elementAverageMass
  , massNumber
  -- * 'elements' - a map containing isotopic data for each element.
  , elements
  -- * Functions taking an 'elementSymbol' as input
  , lookupElement
  , findElement
  , elementName
  , atomicNumber
  , isotopes
  , mostAbundantIsotope
  , selectIsotope
  , isotopicMasses
  , integerMasses
  , isotopicAbundances
  -- * 'ChemicalMass' type class
  , ChemicalMass
  -- * Molecular formulae
  , MolecularFormula
  , emptyMolecularFormula
  , FormulaMult
  , (|+|)
  , (|-|)
  , combineElementSymbolMaps
  , multMolecularFormula
  , renderMolecularFormula
  -- * Condensed formulae
  , CondensedFormula
  , renderCondensedFormula
  ) where

import Isotope.Base
import Isotope.Parsers()
import Prelude hiding (lookup)
