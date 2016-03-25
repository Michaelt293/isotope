{-|
Module      : Isotopic.Element
Description : Contains most of the data type declarations used in the Isotope
              library.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental

This module defines the majority of the types used in the Isotope library. A
large number of type synonyms are provided to improve readability. Of
particular importance are the 'Isotope', 'Element', 'ElementSymbol' and
'ElementSymbolMap' types. 'ElementSymbol' is an enumeration type of all the
element symbols used in the Isotopes library. 'ElementSymbolMap' is a
polymorphic data type which maps an 'ElementSymbol' to a value of some type.
An 'ElementSymbolMap' is used in "Isotope.Periodic" to provide a mapping to
values of type 'Element'. (The type 'Element' has a field of list of 'Isotope'.)
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
module Isotope.Element (
    -- * Type synonyms
      IsotopicMass
    , IsotopicAbundance
    , ElementName
    , AtomicNumber
    , ProtonNumber
    , NeutronNumber
    , Nucleons
    , MassNumber
    , IntegerMass
    , MonoisotopicMass
    , NominalMass
    , AverageMass
    -- * Algebraic data types
    , Isotope(..)
    , Element(..)
    , ElementSymbol(..)
    -- * Newtype
    , ElementSymbolMap(..)
    -- * Type class
    , Mass(..)
    -- * List of ElementSymbol
    , elementSymbolList
    -- * Functions
    , mkElementSymbolMap
    , lookup
    , (!)
    , elementMostAbundantIsotope
    , elementIsotopicMasses
    , elementIntegerMasses
    , elementIsotopicAbundances
    , massNumber
    ) where

import Prelude hiding      (lookup)
import Data.List           (elemIndex)
import Data.Maybe          (fromJust)
import Data.Map            (Map, fromList)
import qualified Data.Map as Map

-- | The exact mass of an isotope.
type IsotopicMass      = Double

-- | The natural abundance of an isotope.
type IsotopicAbundance = Double

-- | The name of an element.
type ElementName       = String

-- | Atomic number of an element.
type AtomicNumber      = Int

-- | Proton number (i.e., the number of protons) for an element/isotope.
type ProtonNumber      = AtomicNumber

-- | Neutron number (i.e., the number of neutrons) for an element.
type NeutronNumber     = Int

-- | Type synonym for a pair containing 'ProtonNumber' and 'NeutronNumber'.
type Nucleons          = (ProtonNumber, NeutronNumber)

-- | The number of protons plus the number of neutrons (i.e., proton number +
-- neutron number) for an isotope.
type MassNumber        = Int

-- | Integer mass for an isotope.
type IntegerMass       = MassNumber

-- | The exact mass of the most abundant isotope for an element or the sum of
-- the exact masses of the most abundant isotope of each element for a
-- chemical formula.
type MonoisotopicMass  = Double

-- | The integer mass of the most abundant isotope for an element or the sum of
-- integer mass of the most abundant isotope of each element for a chemical
-- formula.
type NominalMass       = Int

-- | The average mass of an element or chemical formula based on
-- naturally-occurring abundances.
type AverageMass       = Double

-- | An 'Isotope' has three parameters; 'Nucleons', 'IsotopeMass' and
-- 'IsotopicAbundance'.
data Isotope = Isotope { nucleons          :: Nucleons
                       , isotopicMass      :: IsotopicMass
                       , isotopicAbundance :: IsotopicAbundance
                       } deriving (Show, Eq, Ord)

-- | An 'Element' has three parameters; 'AtomicNumber', 'ElementName' and
-- list of 'Isotope'.
data Element = Element { atomicNumber' :: AtomicNumber
                       , elementName'  :: ElementName
                       , isotopes'     :: [Isotope]
                       } deriving (Show, Eq, Ord)

-- | Element symbols as an enumeration type.
data ElementSymbol = H  | He | Li | Be | B  | C  | N  | O  | F  | Ne | Na | Mg |
                     Al | Si | P  | S  | Cl | Ar | K  | Ca | Sc | Ti | V  | Cr |
                     Mn | Fe | Co | Ni | Cu | Zn | Ga | Ge | As | Se | Br | Kr |
                     Rb | Sr | Y  | Zr | Nb | Mo | Tc | Ru | Rh | Pd | Ag | Cd |
                     In | Sn | Sb | Te | I  | Xe | Cs | Ba | La | Ce | Pr | Nd |
                     Pm | Sm | Eu | Gd | Tb | Dy | Ho | Er | Tm | Yb | Lu | Hf |
                     Ta | W  | Re | Os | Ir | Pt | Au | Hg | Tl | Pb | Bi | Th |
                     Pa | U  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | List containing all element symbols.
elementSymbolList :: [ElementSymbol]
elementSymbolList = [H .. U]

-- | An 'ElementSymbolMap' is a polymorphic datatype mapping an 'ElementSymbol'
-- to some type. 'ElementSymbolMap' is an instance 'Functor', 'Treversable' and
-- 'Foldable' type classes.
newtype ElementSymbolMap a =
    ElementSymbolMap {getSymbolMap :: Map ElementSymbol a}
    deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)

-- | Function to make an 'ElementSymbolMap'.
mkElementSymbolMap :: [(ElementSymbol, a)] -> ElementSymbolMap a
mkElementSymbolMap = ElementSymbolMap . fromList

-- | Polymorphic lookup function to get a value from an 'ElementSymbolMap'.
lookup :: ElementSymbol -> ElementSymbolMap a -> Maybe a
lookup k m = Map.lookup k (getSymbolMap m)

-- | Polymorphic lookup function to get a value from an 'ElementSymbolMap'.
(!) :: ElementSymbolMap a -> ElementSymbol -> a
m ! k = getSymbolMap m Map.! k

-- | Class containing three methods; 'monoisotopicMass', 'nominalMass' and
-- 'averageMass'.
class Mass a where
     monoisotopicMass :: a -> MonoisotopicMass
     nominalMass      :: a -> NominalMass
     averageMass      :: a -> AverageMass

instance Mass Element where
   monoisotopicMass = isotopicMass . elementMostAbundantIsotope
   nominalMass = massNumber . nucleons . elementMostAbundantIsotope
   averageMass e = sum [isotopicMass x * isotopicAbundance x |
                                x <- isotopes' e]

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

-- | Mass number for an isotope. Mass number is the number of protons plus the
-- number of neutrons.
massNumber :: (ProtonNumber, NeutronNumber) -> MassNumber
massNumber (protonNum, neutronNum) = protonNum + neutronNum
