{-|
Module      : Isotope
Description : Isotope is a chemistry library for calculating masses of elements
              and molecules.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental

Import this module to work with the Isotope library. Isotope provides a range of
data types, functions and class abstractions for working with elements and
molecules. The main focus of Isotope is the calculation of element/molecular
masses. For more information regarding Isotope, please refer to the README.
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
  , Isotope(..)
  , Element(..)
  -- * Element symbols
  , ElementSymbol(..)
  , elementSymbolList
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
  , ChemicalMass(..)
  -- * Molecular formulae
  , MolecularFormula(..)
  , (|+|)
  , (|-|)
  , (|*|)
  , FormulaHelperClass(..)
  , Formula(..)
  -- * Condensed formulae
  , CondensedFormula(..)
  , EmpiricalFormula
  , ToEmpiricalFormula(..)
  ) where

import Isotope.Base
import Isotope.Parsers()
import Prelude hiding (lookup)
