{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
module Isotope.Element
    ( IsotopicMass
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
    , Isotope(..)
    , Element(..)
    , ElementSymbol(..)
    , elementSymbolList
    , ElementSymbolMap(..)
    , mkElementSymbolMap
    , lookup
    , (!)
    , Mass(..)
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

type IsotopicMass      = Double
type IsotopicAbundance = Double
type ElementName       = String
type AtomicNumber      = Int
type ProtonNumber      = AtomicNumber
type NeutronNumber     = Int
type Nucleons          = (ProtonNumber, NeutronNumber)
type MassNumber        = Int
type IntegerMass       = Int
type MonoisotopicMass  = Double
type NominalMass       = Int
type AverageMass       = Double

data Isotope = Isotope { nucleons          :: Nucleons
                       , isotopicMass      :: IsotopicMass
                       , isotopicAbundance :: IsotopicAbundance
                       } deriving (Show, Eq, Ord)

data Element = Element { atomicNumber' :: AtomicNumber
                       , elementName'  :: ElementName
                       , isotopes'     :: [Isotope]
                       } deriving (Show, Eq, Ord)

data ElementSymbol = H  | He | Li | Be | B  | C  | N  | O  | F  | Ne | Na | Mg |
                     Al | Si | P  | S  | Cl | Ar | K  | Ca | Sc | Ti | V  | Cr |
                     Mn | Fe | Co | Ni | Cu | Zn | Ga | Ge | As | Se | Br | Kr |
                     Rb | Sr | Y  | Zr | Nb | Mo | Tc | Ru | Rh | Pd | Ag | Cd |
                     In | Sn | Sb | Te | I  | Xe | Cs | Ba | La | Ce | Pr | Nd |
                     Pm | Sm | Eu | Gd | Tb | Dy | Ho | Er | Tm | Yb | Lu | Hf |
                     Ta | W  | Re | Os | Ir | Pt | Au | Hg | Tl | Pb | Bi | Th |
                     Pa | U  deriving (Show, Read, Eq, Ord, Enum, Bounded)

elementSymbolList :: [ElementSymbol]
elementSymbolList = [H .. U]

newtype ElementSymbolMap a = ElementSymbolMap
                           {getSymbolMap :: Map ElementSymbol a}
                           deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)

mkElementSymbolMap :: [(ElementSymbol, a)] -> ElementSymbolMap a
mkElementSymbolMap = ElementSymbolMap . fromList

lookup :: ElementSymbol -> ElementSymbolMap a -> Maybe a
lookup k m = Map.lookup k (getSymbolMap m)

(!) :: ElementSymbolMap a -> ElementSymbol -> a
m ! k = getSymbolMap m Map.! k

class Mass a where
     monoisotopicMass :: a -> MonoisotopicMass
     nominalMass      :: a -> NominalMass
     averageMass      :: a -> AverageMass

instance Mass Element where
   monoisotopicMass = isotopicMass . elementMostAbundantIsotope
   nominalMass = massNumber . nucleons . elementMostAbundantIsotope
   averageMass e = sum [isotopicMass x * isotopicAbundance x |
                                x <- isotopes' e]

-- |Returns the most abundant naturally-occurring isotope for an eleElementment.
elementMostAbundantIsotope :: Element -> Isotope
elementMostAbundantIsotope e = isotopeList !! indexOfIsotope
    where isotopeList = isotopes' e
          abundances = isotopicAbundance <$> isotopeList
          indexOfIsotope = fromJust $ elemIndex (maximum abundances) abundances

-- |Exact masses for all naturally-occurring isotopes for an element.
elementIsotopicMasses :: Element -> [IsotopicMass]
elementIsotopicMasses e = isotopicMass <$> isotopes' e

-- |Integer masses for all naturally-occurring isotopes for an element.
elementIntegerMasses :: Element -> [IntegerMass]
elementIntegerMasses e = massNumber . nucleons <$> isotopes' e

-- |Isotope abundances for all naturally-occurring isotopes for an element.
elementIsotopicAbundances :: Element -> [IsotopicAbundance]
elementIsotopicAbundances e = isotopicAbundance <$> isotopes' e

-- |Mass number for an isotope. Mass number is the number of protons plus the
-- number of neutrons.
massNumber :: (ProtonNumber, NeutronNumber) -> MassNumber
massNumber (protonNum, neutronNum) = protonNum + neutronNum
