{-|
Module      : Isotope.Base
Description : Contains most of the data type declarations used in the Isotope
              library.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental

This module defines the majority of the types used in the Isotope library. A
large number of type synonyms are provided to improve readability. Of particular
importance are the 'Isotope', 'Element', 'ElementSymbol', `ElementalComposition`,
`MolecularFormula`,'CondensedFormula' and `EmpiricalFormula` data types.
'ElementSymbol' is an enumeration type of all the element symbols used in the
Isotopes library.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK hide #-}
module Isotope.Base (
    -- Infix operators used in `Isotope`
      Operators(..)
    -- Types for masses
    , IntegerMass
    , MonoisotopicMass(..)
    , NominalMass(..)
    , AverageMass(..)
    , IsotopicMass(..)
    -- Other types
    , ElementName
    , IsotopicAbundance(..)
    , AtomicNumber
    , ProtonNumber
    , NeutronNumber
    , Nucleons
    , MassNumber
    -- 'Isotope' and 'Element' data types
    , Isotope(..)
    , Element(..)
    -- Element symbols
    , ElementSymbol(..)
    , elementSymbolList
    -- Functions taking an 'Element' as input
    , elementMostAbundantIsotope
    , elementIsotopicMasses
    , elementIntegerMasses
    , elementIsotopicAbundances
    , elementMonoisotopicMass
    , elementNominalMass
    , elementAverageMass
    , massNumber
    -- 'elements' - a map containing isotopic data for each element.
    , elements
    -- Functions taking an 'elementSymbol' as input
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
    -- Formula type class
    , Formula(..)
    -- Elemental composition
    , ElementalComposition(..)
    , ToElementalComposition(..)
    , mkElementalComposition
    -- Molecular formulae
    , MolecularFormula(..)
    , ToMolecularFormula(..)
    , mkMolecularFormula
    -- Condensed formulae
    , CondensedFormula(..)
    , ToCondensedFormuala(..)
    -- Empirical formula
    , EmpiricalFormula(..)
    , ToEmpiricalFormula(..)
    , mkEmpiricalFormula
    ) where

import Prelude hiding      (lookup,filter)
import Data.Map            ( Map
                           , fromList
                           , unionWith
                           , filter
                           , mapWithKey
                           , lookup
                           , (!)
                           , toList
                           )
import Data.Foldable hiding (toList)
import Data.Ord
import Data.List           (elemIndex, sortBy)
import Data.Maybe          (fromJust)
import Data.Monoid

--------------------------------------------------------------------------------

-- | Infix operators used in `Isotope`. These operators support addition,
-- subtraction and multiplication.
class Operators a where
  (|+|) :: a -> a -> a
  (|-|) :: a -> a -> a
  (|*|) :: a -> Int -> a
  infixl 6 |+|
  infixl 7 |*|
  infixl 6 |-|

--------------------------------------------------------------------------------
-- Types for masses

-- | Integer mass for an isotope.
type IntegerMass = MassNumber

-- | The exact mass of the most abundant isotope for an element or the sum of
-- the exact masses of the most abundant isotope of each element for a
-- molecular formula.
newtype MonoisotopicMass = MonoisotopicMass { getMonoisotopicMass :: Double }
                         deriving (Show, Eq, Ord)

instance Monoid MonoisotopicMass where
  mempty = MonoisotopicMass 0
  mappend = (|+|)

instance Operators MonoisotopicMass where
  MonoisotopicMass x |+| MonoisotopicMass y = MonoisotopicMass $ x + y
  MonoisotopicMass x |-| MonoisotopicMass y = MonoisotopicMass $ x - y
  MonoisotopicMass x |*| y = MonoisotopicMass $ x * fromIntegral y

-- | The integer mass of the most abundant isotope for an element or the sum of
-- integer mass of the most abundant isotope of each element for a chemical
-- formula.
newtype NominalMass = NominalMass { getNominalMass :: Int }
                    deriving (Show, Eq, Ord)

instance Monoid NominalMass where
  mempty = NominalMass 0
  mappend = (|+|)

instance Operators NominalMass where
  NominalMass x |+| NominalMass y = NominalMass $ x + y
  NominalMass x |-| NominalMass y = NominalMass $ x - y
  NominalMass x |*| y = NominalMass $ x * y

-- | The average mass of an element or molecular formula based on
-- naturally-occurring abundances.
newtype AverageMass = AverageMass { getAverageMass :: Double }
                    deriving (Show, Eq, Ord)

instance Monoid AverageMass where
  mempty = AverageMass 0
  mappend = (|+|)

instance Operators AverageMass where
  AverageMass x |+| AverageMass y = AverageMass $ x + y
  AverageMass x |-| AverageMass y = AverageMass $ x - y
  AverageMass x |*| y = AverageMass $ x * fromIntegral y

-- | The exact mass of an isotope.
newtype IsotopicMass = IsotopicMass { getIsotopicMass :: Double }
                     deriving (Show, Eq, Ord)

instance Monoid IsotopicMass where
  mempty = IsotopicMass 0
  mappend = (|+|)

instance Operators IsotopicMass where
  IsotopicMass x |+| IsotopicMass y = IsotopicMass $ x + y
  IsotopicMass x |-| IsotopicMass y = IsotopicMass $ x - y
  IsotopicMass x |*| y = IsotopicMass $ x * fromIntegral y

--------------------------------------------------------------------------------
-- Other types

-- | The name of an element.
type ElementName       = String

-- | The natural abundance of an isotope.
newtype IsotopicAbundance = IsotopicAbundance { getIsotopicAbundance :: Double }
                          deriving (Show, Eq, Ord)

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

--------------------------------------------------------------------------------

-- 'Isotope' and 'Element' data types

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

--------------------------------------------------------------------------------
-- Element symbols

-- | Element symbols as an enumeration type.
data ElementSymbol = H  | He | Li | Be | B  | C  | N  | O  | F  | Ne | Na | Mg |
                     Al | Si | P  | S  | Cl | Ar | K  | Ca | Sc | Ti | V  | Cr |
                     Mn | Fe | Co | Ni | Cu | Zn | Ga | Ge | As | Se | Br | Kr |
                     Rb | Sr | Y  | Zr | Nb | Mo | Ru | Rh | Pd | Ag | Cd | In |
                     Sn | Sb | Te | I  | Xe | Cs | Ba | La | Ce | Pr | Nd | Sm |
                     Eu | Gd | Tb | Dy | Ho | Er | Tm | Yb | Lu | Hf | Ta | W  |
                     Re | Os | Ir | Pt | Au | Hg | Tl | Pb | Bi | Th | Pa | U
                     deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | List containing all element symbols.
elementSymbolList :: [ElementSymbol]
elementSymbolList = [H .. U]

--------------------------------------------------------------------------------
-- Functions taking an 'Element' as input

-- | Returns the most abundant naturally-occurring isotope for an element.
elementMostAbundantIsotope :: Element -> Isotope
elementMostAbundantIsotope e =
  maximumBy (comparing isotopicAbundance) $ isotopes' e

-- | Exact masses for all naturally-occurring isotopes for an element.
elementIsotopicMasses :: Element -> [IsotopicMass]
elementIsotopicMasses e = isotopicMass <$> isotopes' e

-- | Integer masses for all naturally-occurring isotopes for an element.
elementIntegerMasses :: Element -> [IntegerMass]
elementIntegerMasses e = massNumber . nucleons <$> isotopes' e

-- | Isotope abundances for all naturally-occurring isotopes for an element.
elementIsotopicAbundances :: Element -> [IsotopicAbundance]
elementIsotopicAbundances e = isotopicAbundance <$> isotopes' e

-- | Monoistopic mass for an element.
elementMonoisotopicMass :: Element -> MonoisotopicMass
elementMonoisotopicMass =
  MonoisotopicMass . getIsotopicMass . isotopicMass . elementMostAbundantIsotope

-- | Nominal mass for an element.
elementNominalMass :: Element -> NominalMass
elementNominalMass =
  NominalMass . massNumber . nucleons . elementMostAbundantIsotope

-- | Average mass of an element.
elementAverageMass :: Element -> AverageMass
elementAverageMass e =
  foldMap (\x -> AverageMass $
    getIsotopicMass (isotopicMass x) *
    getIsotopicAbundance (isotopicAbundance x))
    (isotopes' e)

-- Mass number for an isotope. Mass number is the number of protons plus the
-- number of neutrons.
massNumber :: Nucleons -> MassNumber
massNumber (protonNum, neutronNum) = protonNum + neutronNum
--------------------------------------------------------------------------------
-- 'elements' - a map containing isotopic data for each element.

-- Helper function to convert a tuple to an `Isotope`.
tupleToIsotope :: (Nucleons, Double, Double) -> Isotope
tupleToIsotope (nucl, mass, abun) =
  Isotope nucl (IsotopicMass mass) (IsotopicAbundance abun)

tupleToElement :: (AtomicNumber, ElementName, [(Nucleons, Double, Double)]) -> Element
tupleToElement (atomNum, name, isotopes) =
  Element atomNum name (tupleToIsotope <$> isotopes)

-- | Map of the periodic table. All data on isotopic masses and abundances is
-- contained within this map.
elements :: Map ElementSymbol Element
elements = tupleToElement <$> fromList
  [ (H,  (1, "hydrogen",      [ ((1, 0),     1.00782503223,  0.999885)
                              , ((1, 1),     2.01410177812,  0.000115) ]))
  , (He, (2,  "helium",       [ ((2, 1),     3.0160293201,   0.00000134)
                              , ((2, 2),     4.00260325413,  0.99999866) ]))
  , (Li, (3,  "lithium",      [ ((3, 3),     6.0151228874,   0.0759)
                              , ((3, 4),     7.0160034366,   0.9241) ]))
  , (Be, (4,  "beryllium",    [ ((4, 5),     9.012183065,    1.0) ]))
  , (B,  (5,  "boron",        [ ((5, 5),     10.01293695,    0.199)
                              , ((5, 6),     11.00930536,    0.801) ]))
  , (C,  (6,  "carbon",       [ ((6, 6),     12.0000000,     0.9893)
                              , ((6, 7),     13.00335483507, 0.0107) ]))
  , (N,  (7,  "nitrogen",     [ ((7, 7),     14.00307400443, 0.99636)
                              , ((7, 8),     15.00010889888, 0.00364) ]))
  , (O,  (8,  "oxygen",       [ ((8, 8),     15.99491461957, 0.99757)
                              , ((8, 9),     16.99913175650, 0.00038)
                              , ((8, 10),    17.99915961286, 0.00205) ]))
  , (F,  (9,  "fluorine",     [ ((9, 10),    18.99840316273, 1.0) ]))
  , (Ne, (10, "neon",         [ ((10, 10),   19.9924401762,  0.9048)
                              , ((10, 11),   20.993846685,   0.0027)
                              , ((10, 12),   21.991385114,   0.0925) ]))
  , (Na, (11, "sodium",       [ ((11, 12),   22.9897692820,  1.0) ]))
  , (Mg, (12, "magnesium",    [ ((12, 12),   23.985041697,   0.7899)
                              , ((12, 13),   24.985836976,   0.1000)
                              , ((12, 14),   25.982592968,   0.1101) ]))
  , (Al, (13, "aluminium",    [ ((13, 14),   26.98153853,    1.0) ]))
  , (Si, (14, "silicon",      [ ((14, 14),   27.97692653465, 0.92223)
                              , ((14, 15),   28.97649466490, 0.04685)
                              , ((14, 16),   29.973770136,   0.03092) ]))
  , (P,  (15, "phosphorous",  [ ((15, 16),   30.97376199842, 1.0) ]))
  , (S,  (16, "sulfur",       [ ((16, 16),   31.9720711744,  0.9499)
                              , ((16, 17),   32.9714589098,  0.0075)
                              , ((16, 18),   33.967867004,   0.0425)
                              , ((16, 20),   35.96708071,    0.0001) ]))
  , (Cl, (17, "chlorine",     [ ((17, 18),   34.968852682,   0.7576)
                              , ((17, 20),   36.965902602,   0.2424) ]))
  , (Ar, (18, "argon",        [ ((18, 18),   35.967545105,   0.003336)
                              , ((18, 20),   37.96273211,    0.000629)
                              , ((18, 22),   39.9623831237,  0.996035) ]))
  , (K,  (19, "potassium",    [ ((19, 20),   38.9637064864,  0.932581)
                              , ((19, 21),   39.963998166,   0.000117)
                              , ((19, 22),   40.9618252579,  0.067302) ]))
  , (Ca, (20, "calcium",      [ ((20, 20),   39.962590863,   0.96941)
                              , ((20, 22),   41.95861783,    0.00647)
                              , ((20, 23),   42.95876644,    0.00135)
                              , ((20, 24),   43.95548156,    0.02086)
                              , ((20, 26),   45.9536890,     0.00004)
                              , ((20, 28),   47.95252276,    0.00187) ]))
  , (Sc, (21, "scandium",     [ ((21, 24),   44.95590828,    1.0) ]))
  , (Ti, (22, "titanium",     [ ((22, 24),   45.95262772,    0.0825)
                              , ((22, 25),   46.95175879,    0.0744)
                              , ((22, 26),   47.94794198,    0.7372)
                              , ((22, 27),   48.94786568,    0.0541)
                              , ((22, 28),   49.94478689,    0.0518) ]))
  , (V,  (23, "vanadium",     [ ((23, 27),   49.94715601,    0.00250)
                              , ((23, 28),   50.94395704,    0.99750) ]))
  , (Cr, (24, "chromium",     [ ((24, 26),   49.94604183,    0.04345)
                              , ((24, 28),   51.94050623,    0.83789)
                              , ((24, 29),   52.94064815,    0.09501)
                              , ((24, 30),   53.93887916,    0.02365) ]))
  , (Mn, (25, "manganese",    [ ((25, 30),   54.93804391,    1.0) ]))
  , (Fe, (26, "iron",         [ ((26, 28),   53.93960899,    0.05845)
                              , ((26, 30),   55.93493633,    0.91754)
                              , ((26, 31),   56.93539284,    0.02119)
                              , ((26, 32),   57.93327443,    0.00282) ]))
  , (Co, (27, "cobalt",       [ ((27, 32),   58.93319429,    1.0) ]))
  , (Ni, (28, "nickel",       [ ((28, 30),   57.93534241,    0.68077)
                              , ((28, 32),   59.93078588,    0.26223)
                              , ((28, 33),   60.93105557,    0.011399)
                              , ((28, 34),   61.92834537,    0.036346)
                              , ((28, 36),   63.92796682,    0.009255) ]))
  , (Cu, (29, "copper",       [ ((29, 34),   62.92959772,    0.6915)
                              , ((29, 36),   64.92778970,    0.3085) ]))
  , (Zn, (30, "zinc",         [ ((30, 34),   63.92914201,    0.4917)
                              , ((30, 36),   65.92603381,    0.2773)
                              , ((30, 37),   66.92712775,    0.0404)
                              , ((30, 38),   67.92484455,    0.1845)
                              , ((30, 40),   69.9253192,     0.0061) ]))
  , (Ga, (31, "gallium",      [ ((31, 38),   68.9255735,     0.60108)
                              , ((31, 40),   70.92470258,    0.39892) ]))
  , (Ge, (32, "germanium",    [ ((32, 38),   69.92424875,    0.2057)
                              , ((32, 40),   71.922075826,   0.2745)
                              , ((32, 41),   72.923458956,   0.0775)
                              , ((32, 42),   73.921177761,   0.3650)
                              , ((32, 44),   75.921402726,   0.0773) ]))
  , (As, (33, "arsenic",      [ ((33, 42),   74.92159457,    1.0) ]))
  , (Se, (34, "selenium",     [ ((34, 40),   73.922475934,   0.0089)
                              , ((34, 42),   75.919213704,   0.0937)
                              , ((34, 43),   76.919914154,   0.0763)
                              , ((34, 44),   77.91730928,    0.2377)
                              , ((34, 46),   79.9165218,     0.4961)
                              , ((34, 48),   81.9166995,     0.0873) ]))
  , (Br, (35, "bromine",      [ ((35, 44),   78.9183376,     0.5069)
                              , ((35, 46),   80.9162897,     0.4931) ]))
  , (Kr, (36, "krypton",      [ ((36, 42),   77.92036494,    0.00355)
                              , ((36, 44),   79.91637808,    0.02286)
                              , ((36, 46),   81.91348273,    0.11593)
                              , ((36, 47),   82.91412716,    0.11500)
                              , ((36, 48),   83.9114977282,  0.56987)
                              , ((36, 50),   85.9106106269,  0.17279) ]))
  , (Rb, (37, "rubidium",     [ ((37, 48),   84.9117897379,  0.7217)
                              , ((37, 50),   86.9091805310,  0.2783) ]))
  , (Sr, (38, "strontium",    [ ((38, 46),   83.9134191,     0.0056)
                              , ((38, 48),   85.9092606,     0.0986)
                              , ((38, 49),   86.9088775,     0.0700)
                              , ((38, 50),   87.9056125,     0.8258) ]))
  , (Y,  (39, "yttrium",      [ ((39, 50),   88.9058403,     1.0) ]))
  , (Zr, (40, "zirconium",    [ ((40, 50),   89.9046977,     0.5145)
                              , ((40, 51),   90.9056396,     0.1122)
                              , ((40, 52),   91.9050347,     0.1715)
                              , ((40, 54),   93.9063108,     0.1738)
                              , ((40, 56),   95.9082714,     0.0280) ]))
  , (Nb, (41, "niobium",      [ ((41, 52),   92.9063730,     1.0) ]))
  , (Mo, (42, "molybdenum",   [ ((42, 50),   91.90680796,    0.1453)
                              , ((42, 52),   93.90508490,    0.0915)
                              , ((42, 53),   94.90583877,    0.1584)
                              , ((42, 54),   95.90467612,    0.1667)
                              , ((42, 55),   96.90601812,    0.0960)
                              , ((42, 56),   97.90540482,    0.2439)
                              , ((42, 58),   99.9074718,     0.0982) ]))
  , (Ru, (44, "ruthenium",    [ ((44, 52),   95.90759025,    0.0554)
                              , ((44, 54),   97.9052868,     0.0187)
                              , ((44, 55),   98.9059341,     0.1276)
                              , ((44, 56),   99.9042143,     0.1260)
                              , ((44, 57),   100.9055769,    0.1706)
                              , ((44, 58),   101.9043441,    0.3155)
                              , ((44, 59),   103.9054275,    0.1862) ]))
  , (Rh, (45, "rhodium",      [ ((45, 58),   102.9054980,    1.0) ]))
  , (Pd, (46, "palladium",    [ ((46, 56),   101.9056022,    0.0102)
                              , ((46, 58),   103.9040305,    0.1114)
                              , ((46, 59),   104.9050796,    0.2233)
                              , ((46, 60),   105.9034804,    0.2733)
                              , ((46, 62),   107.9038916,    0.2646)
                              , ((46, 64),   109.90517220,   0.1172) ]))
  , (Ag, (47, "silver",       [ ((47, 60),   106.9050916,    0.51839)
                              , ((47, 62),   108.9047553,    0.48161) ]))
  , (Cd, (48, "cadmium",      [ ((48, 58),   105.9064599,    0.0125)
                              , ((48, 60),   107.9041834,    0.0089)
                              , ((48, 62),   109.90300661,   0.1249)
                              , ((48, 63),   110.90418287,   0.1280)
                              , ((48, 64),   111.90276287,   0.2413)
                              , ((48, 65),   112.90440813,   0.1222)
                              , ((48, 66),   113.90336509,   0.2873)
                              , ((48, 68),   115.90476315,   0.0749) ]))
  , (In, (49, "indium",       [ ((49, 64),   112.90406184,   0.0429)
                              , ((49, 66),   114.903878776,  0.9571) ]))
  , (Sn, (50, "tin",          [ ((50, 62),   111.90482387,   0.0097)
                              , ((50, 64),   113.9027827,    0.0066)
                              , ((50, 65),   114.903344699,  0.0034)
                              , ((50, 66),   115.90174280,   0.1454)
                              , ((50, 67),   116.90295398,   0.0768)
                              , ((50, 68),   117.90160657,   0.2422)
                              , ((50, 69),   118.90331117,   0.0859)
                              , ((50, 70),   119.90220163,   0.3258)
                              , ((50, 72),   121.9034438,    0.0463)
                              , ((50, 74),   123.9052766,    0.0579) ]))
  , (Sb, (51, "antimony",     [ ((51, 70),   120.9038120,    0.5721)
                              , ((51, 72),   122.9042132,    0.4279) ]))
  , (Te, (52, "tellurium",    [ ((52, 68),   119.9040593,    0.0009)
                              , ((52, 70),   121.9030435,    0.0255)
                              , ((52, 71),   122.9042698,    0.0089)
                              , ((52, 72),   123.9028171,    0.0474)
                              , ((52, 73),   124.9044299,    0.0707)
                              , ((52, 74),   125.9033109,    0.1884)
                              , ((52, 76),   127.90446128,   0.3174)
                              , ((52, 78),   129.906222748,  0.3408) ]))
  , (I,  (53, "iodine",       [ ((53, 74),   126.9044719,    1.0) ]))
  , (Xe, (54, "xenon",        [ ((54, 70),   123.9058920,    0.000952)
                              , ((54, 72),   125.9042983,    0.000890)
                              , ((54, 74),   127.9035310,    0.019102)
                              , ((54, 75),   128.9047808611, 0.264006)
                              , ((54, 76),   129.903509349,  0.040710)
                              , ((54, 77),   130.90508406,   0.212324)
                              , ((54, 78),   131.9041550856, 0.269086)
                              , ((54, 80),   133.90539466,   0.104357)
                              , ((54, 82),   135.907214484,  0.088573) ]))
  , (Cs, (55, "caesium",      [ ((55, 78),   132.9054519610, 1.0) ]))
  , (Ba, (56, "barium",       [ ((56, 74),   129.9063207,    0.00106)
                              , ((56, 76),   131.9050611,    0.00101)
                              , ((56, 78),   133.90450818,   0.02417)
                              , ((56, 79),   134.90568838,   0.06592)
                              , ((56, 80),   135.90457573,   0.07854)
                              , ((56, 81),   136.90582714,   0.11232)
                              , ((56, 82),   137.90524700,   0.71698) ]))
  , (La, (57, "lanthanum",    [ ((57, 81),   137.9071149,    0.0008881)
                              , ((57, 83),   138.9063563,    0.9991119) ]))
  , (Ce, (58, "cerium",       [ ((58, 78),   135.90712921,   0.00185)
                              , ((58, 80),   137.905991,     0.00251)
                              , ((58, 82),   139.9054431,    0.88450)
                              , ((58, 84),   141.9092504,    0.11114) ]))
  , (Pr, (59, "praseodymium", [ ((59, 82),   140.9076576,    1.0) ]))
  , (Nd, (60, "neodymium",    [ ((60, 82),   141.9077290,    0.27152)
                              , ((60, 83),   142.9098200,    0.12174)
                              , ((60, 84),   143.9100930,    0.23798)
                              , ((60, 85),   144.9125793,    0.08293)
                              , ((60, 86),   145.9131226,    0.17189)
                              , ((60, 88),   147.9168993,    0.05756)
                              , ((60, 90),   149.9209022,    0.05638) ]))
  , (Sm, (62, "samarium",     [ ((62, 82),   143.9120065,    0.0307)
                              , ((62, 83),   146.9149044,    0.1499)
                              , ((62, 84),   147.9148292,    0.1124)
                              , ((62, 85),   148.9171921,    0.1382)
                              , ((62, 86),   149.9172829,    0.0738)
                              , ((62, 88),   151.9197397,    0.2675)
                              , ((62, 90),   153.9222169,    0.2275) ]))
  , (Eu, (63, "europium",     [ ((63, 88),   150.9198578,    0.4781)
                              , ((63, 90),   152.9212380,    0.5219) ]))
  , (Gd, (64, "gadolinium",   [ ((64, 88),   151.9197995,    0.0020)
                              , ((64, 90),   153.9208741,    0.0218)
                              , ((64, 91),   154.9226305,    0.1480)
                              , ((64, 92),   155.9221312,    0.2047)
                              , ((64, 93),   156.9239686,    0.1565)
                              , ((64, 94),   157.9241123,    0.2484)
                              , ((64, 96),   159.9270624,    0.2186) ]))
  , (Tb, (65, "terbium",      [ ((65, 94),   158.9253547,    1.0) ]))
  , (Dy, (66, "dysprosium",   [ ((66, 90),   155.9242847,    0.00056)
                              , ((66, 92),   157.9244159,    0.00095)
                              , ((66, 94),   159.9252046,    0.02329)
                              , ((66, 95),   160.9269405,    0.18889)
                              , ((66, 96),   161.9268056,    0.25475)
                              , ((66, 97),   162.9287383,    0.24896)
                              , ((66, 98),   163.9291819,    0.28260) ]))
  , (Ho, (67, "holmium",      [ ((67, 98),   164.9303288,    1.0) ]))
  , (Er, (68, "erbium",       [ ((68, 94),   161.9287884,    0.00139)
                              , ((68, 96),   163.9292088,    0.01601)
                              , ((68, 98),   165.9302995,    0.33503)
                              , ((68, 99),   166.9320546,    0.22869)
                              , ((68, 100),  167.9323767,    0.26978)
                              , ((68, 102),  169.9354702,    0.14910) ]))
  , (Tm, (69, "thulium",      [ ((69, 100),  168.9342179,    1.0) ]))
  , (Yb, (70, "ytterbium",    [ ((70, 98),   167.9338896,    0.00123)
                              , ((70, 100),  169.9347664,    0.02982)
                              , ((70, 101),  170.9363302,    0.1409)
                              , ((70, 102),  171.9363859,    0.2168)
                              , ((70, 103),  172.9382151,    0.16103)
                              , ((70, 104),  173.9388664,    0.32026)
                              , ((70, 106),  175.9425764,    0.12996) ]))
  , (Lu, (71, "lutetium",     [ ((71, 104),  174.9407752,    0.97401)
                              , ((71, 105),  175.9426897,    0.02599) ]))
  , (Hf, (72, "hafnium",      [ ((72, 102),  173.9400461,    0.0016)
                              , ((72, 104),  175.9414076,    0.0526)
                              , ((72, 105),  176.9432277,    0.1860)
                              , ((72, 106),  177.9437058,    0.2728)
                              , ((72, 107),  178.9458232,    0.1362)
                              , ((72, 108),  179.9465570,    0.3508) ]))
  , (Ta, (73, "tantalum",     [ ((73, 107),  179.9474648,    0.0001201)
                              , ((73, 108),  180.9479958,    0.9998799) ]))
  , (W,  (74, "tungsten",     [ ((74, 106),  179.9467108,    0.0012)
                              , ((74, 108),  181.94820394,   0.2650)
                              , ((74, 109),  182.95022275,   0.1431)
                              , ((74, 110),  183.95093092,   0.3064)
                              , ((74, 112),  185.9543628,    0.2843) ]))
  , (Re, (75, "rhenium",      [ ((75, 110),  184.9529545,    0.3740)
                              , ((75, 112),  186.9557501,    0.6260) ]))
  , (Os, (76, "osmium",       [ ((76, 108),  183.9524885,    0.0002)
                              , ((76, 110),  185.9538350,    0.0159)
                              , ((76, 111),  186.9557474,    0.0196)
                              , ((76, 112),  187.9558352,    0.1324)
                              , ((76, 113),  188.9581442,    0.1615)
                              , ((76, 114),  189.9584437,    0.2626)
                              , ((76, 116),  191.9614770,    0.4078) ]))
  , (Ir, (77, "iridium",      [ ((77, 114),  190.9605893,    0.373)
                              , ((77, 116),  192.9629216,    0.627) ]))
  , (Pt, (78, "platinum",     [ ((78, 112),  189.9599297,    0.00012)
                              , ((78, 114),  191.9610387,    0.00782)
                              , ((78, 116),  193.9626809,    0.3286)
                              , ((78, 117),  194.9647917,    0.3378)
                              , ((78, 118),  195.96495209,   0.2521)
                              , ((78, 120),  197.9678949,    0.07356) ]))
  , (Au, (79, "gold",         [ ((79, 118),  196.96656879,   1.0) ]))
  , (Hg, (80, "mercury",      [ ((80, 116),  195.9658326,    0.0015)
                              , ((80, 118),  197.96676860,   0.0997)
                              , ((80, 119),  198.96828064,   0.1687)
                              , ((80, 120),  199.96832659,   0.2310)
                              , ((80, 121),  200.97030284,   0.1318)
                              , ((80, 122),  201.97064340,   0.2986)
                              , ((80, 124),  203.97349398,   0.0687) ]))
  , (Tl, (81, "thallium",     [ ((81, 122),  202.9723446,    0.2952)
                              , ((81, 124),  204.9744278,    0.7048) ]))
  , (Pb, (82, "lead",         [ ((82, 122),  203.9730440,    0.014)
                              , ((82, 124),  205.9744657,    0.241)
                              , ((82, 125),  206.9758973,    0.221)
                              , ((82, 126),  207.9766525,    0.524) ]))
  , (Bi, (83, "bismuth",      [ ((83, 126),  208.9803991,    1.0) ]))
  , (Th, (90, "thorium",      [ ((90, 142),  232.0380558,    1.0) ]))
  , (Pa, (91, "protactinium", [ ((91, 140),  231.0358842,    1.0) ]))
  , (U,  (92, "uranium",      [ ((92, 142),  234.0409523,    0.000054)
                              , ((92, 143),  235.0439301,    0.007204)
                              , ((92, 146),  238.0507884,    0.992742) ]))
  ]

--------------------------------------------------------------------------------
-- Functions taking an 'elementSymbol' as input

-- | Searches elements (a map) with an 'ElementSymbol' key and returns
-- information for the element (wrapped in 'Maybe').
lookupElement :: ElementSymbol -> Maybe Element
lookupElement = flip lookup elements

-- | Searches 'elements' (a map) with an 'ElementSymbol' key and returns
-- information for the element.
findElement :: ElementSymbol -> Element
findElement = (!) elements

-- | Returns the name for an element symbol.
elementName :: ElementSymbol -> ElementName
elementName = elementName' . findElement

-- | Returns the atomic number for an element.
atomicNumber :: ElementSymbol -> AtomicNumber
atomicNumber = atomicNumber' . findElement

-- | Returns all the naturally-occurring isotopes for an element.
isotopes :: ElementSymbol -> [Isotope]
isotopes = isotopes' . findElement

-- | Returns the most abundant naturally-occurring isotope for an element.
mostAbundantIsotope :: ElementSymbol -> Isotope
mostAbundantIsotope = elementMostAbundantIsotope . findElement

-- | Selects an isotope of element based on the isotope's mass number
-- ('IntegerMass'). Note: This is a partial function.
selectIsotope :: ElementSymbol -> MassNumber -> Isotope
selectIsotope sym mass = isotopeList !! indexOfIsotope
    where isotopeList = isotopes sym
          indexOfIsotope = fromJust $ elemIndex mass (integerMasses sym)

-- | Exact masses for all naturally-occurring isotopes for an element.
isotopicMasses :: ElementSymbol -> [IsotopicMass]
isotopicMasses = elementIsotopicMasses . findElement

-- | Integer masses for all naturally-occurring isotopes for an element.
integerMasses :: ElementSymbol -> [IntegerMass]
integerMasses = elementIntegerMasses . findElement

-- | Isotope abundances for all naturally-occurring isotopes for an element.
isotopicAbundances :: ElementSymbol -> [IsotopicAbundance]
isotopicAbundances = elementIsotopicAbundances . findElement

--------------------------------------------------------------------------------
-- Formula type class

-- | Type class with two methods, 'renderFormula' and 'emptyFormula'. The
-- 'renderFormula' method converts a formula to its shorthand notation.
class Formula a where
    renderFormula :: a -> String
    emptyFormula  :: a

--------------------------------------------------------------------------------
-- Elemental composition

-- | Provided since 'EmpiricalFormula' can be thought of as an elemenal
-- composition but is not a molecular formula.
newtype ElementalComposition = ElementalComposition  {
    getElementalComposition :: Map ElementSymbol Int }
        deriving (Show, Read, Eq, Ord)

-- | Class containing four methods; 'toElementalComposition',
-- 'monoisotopicMass', 'nominalMass' and 'averageMass'.
class ToElementalComposition a where
     toElementalComposition :: a -> ElementalComposition
     monoisotopicMass       :: a -> MonoisotopicMass
     nominalMass            :: a -> NominalMass
     averageMass            :: a -> AverageMass
     monoisotopicMass = getFormulaSum elementMonoisotopicMass
     nominalMass      = getFormulaSum elementNominalMass
     averageMass      = getFormulaSum elementAverageMass
     {-# MINIMAL (toElementalComposition) #-}

-- Helper function for the calculating monoistopic masses, average mass and
-- nominal masses for molecular formulae.
getFormulaSum :: (Monoid a, Operators a, ToElementalComposition b)
  => (Element -> a) -> b -> a
getFormulaSum f m = fold $
    mapWithKey mapFunc (getElementalComposition (toElementalComposition m))
  where mapFunc k v = (f . findElement) k |*| v

-- | Smart constructor to make values of type 'ElementalComposition'.
mkElementalComposition :: [(ElementSymbol, Int)] -> ElementalComposition
mkElementalComposition = ElementalComposition . filterZero . fromList

instance Monoid ElementalComposition where
  mempty = emptyFormula
  mappend = (|+|)

instance Operators ElementalComposition where
  ElementalComposition x |+| ElementalComposition y =
    ElementalComposition $ combineMaps (+) x y
  ElementalComposition x |-| ElementalComposition y =
    ElementalComposition $ combineMaps (-) x y
  m |*| n = ElementalComposition . filterZero $
              (fromIntegral n *) <$> getElementalComposition m

instance ToElementalComposition ElementSymbol where
    toElementalComposition sym = mkElementalComposition [(sym, 1)]

instance ToElementalComposition ElementalComposition where
  toElementalComposition = id

instance Formula ElementalComposition where
   renderFormula f = foldMap renderFoldfunc
                     ((sortElementSymbolMap . getElementalComposition) f)
   emptyFormula    = mkElementalComposition []

-- Helper function for 'renderFormula'.
renderFoldfunc :: (ElementSymbol, Int) -> String
renderFoldfunc (sym, num) = show sym <> if num == 1
                                           then ""
                                           else show num

-- Use the Hill system for writing formulas. C then H followed by elements in
-- alphabetical order.
sortElementSymbolMap :: Map ElementSymbol Int -> [(ElementSymbol, Int)]
sortElementSymbolMap m = sortBy (hillSystem fst) elementSymbolIntList
    where
      elementSymbolIntList = toList m
      elementSymbols = fst <$> elementSymbolIntList
      containsC = C `elem` elementSymbols
      hillSystem f a b = case (f a, f b) of
        (C, _)   -> LT
        (_, C)   -> GT
        (H, b')  -> if containsC then LT
                    else (show . elementName) H `compare` show b'
        (a', H)  -> if containsC then GT
                    else show a' `compare` (show . elementName) H
        (a', b') -> show a' `compare` show b'

--------------------------------------------------------------------------------
-- Molecular formulae

-- | 'MolecularFormula' is a newtype to represent a molecular formula.
newtype MolecularFormula = MolecularFormula {
    getMolecularFormula :: Map ElementSymbol Int }
        deriving (Show, Read, Eq, Ord)

class ToMolecularFormula a where
    toMolecularFormula :: a -> MolecularFormula

-- The function unionWith adapted to work with 'Map ElementSymbol Int'.
combineMaps :: (Int -> Int -> Int)
  -> Map ElementSymbol Int ->  Map ElementSymbol Int  ->  Map ElementSymbol Int
combineMaps f m1 m2 = filterZero $ unionWith f m1 m2

-- | Smart constructor to make values of type 'MolecularFormula'.
mkMolecularFormula :: [(ElementSymbol, Int)] -> MolecularFormula
mkMolecularFormula = MolecularFormula . filterZero . fromList

-- Helper function to remove (k, v) pairs where v == 0.
filterZero :: Map k Int -> Map k Int
filterZero = filter (/= 0)

instance Monoid MolecularFormula where
   mempty = emptyFormula
   mappend = (|+|)

instance Operators MolecularFormula where
  MolecularFormula x |+| MolecularFormula y =
    MolecularFormula $ combineMaps (+) x y
  MolecularFormula x |-| MolecularFormula y =
    MolecularFormula $ combineMaps (-) x y
  m |*| n = MolecularFormula . filterZero $
              (fromIntegral n *) <$> getMolecularFormula m

instance ToElementalComposition MolecularFormula where
    toElementalComposition (MolecularFormula m) = ElementalComposition m

instance Formula MolecularFormula where
   renderFormula f = foldMap renderFoldfunc
                     ((sortElementSymbolMap . getMolecularFormula) f)
   emptyFormula = mkMolecularFormula []

--------------------------------------------------------------------------------
-- Condensed formulae

-- | 'CondensedFormula' is a newtype to represent a condensed formula.
newtype CondensedFormula = CondensedFormula {
    getCondensedFormula :: [Either MolecularFormula (CondensedFormula, Int)] }
        deriving (Show, Read, Eq, Ord)

class ToCondensedFormuala a where
  toCondensedFormula :: a -> CondensedFormula

instance Monoid CondensedFormula where
  mempty = emptyFormula
  CondensedFormula x `mappend` CondensedFormula y = CondensedFormula (x <> y)

instance ToElementalComposition CondensedFormula where
    toElementalComposition =
      ElementalComposition . getMolecularFormula . toMolecularFormula

instance ToMolecularFormula CondensedFormula where
    toMolecularFormula c = foldMap foldFunc (getCondensedFormula c)
       where foldFunc = \case
                         Left chemForm -> chemForm
                         Right (condForm, n) -> toMolecularFormula condForm |*| n

instance Formula CondensedFormula where
    renderFormula c = foldMap foldFunc (getCondensedFormula c)
        where foldFunc = \case
                          Left chemForm -> renderFormula chemForm
                          Right (chemFormList, n) ->
                            "(" <> renderFormula chemFormList <> ")"
                             <> formatNum n
                                where formatNum n' = if n' == 1 then ""
                                                     else show n'
    emptyFormula = CondensedFormula []

--------------------------------------------------------------------------------
-- | 'EmpiricalFormula' is a newtype to represent a empirical formula.
newtype EmpiricalFormula = EmpiricalFormula {
    getEmpiricalFormula :: Map ElementSymbol Int }
        deriving (Show, Read, Eq, Ord)

-- | Type class with a single method, 'toEmpiricalFormula', which converts a
-- chemical data type to `EmpiricalFormula`.
class ToEmpiricalFormula a where
  toEmpiricalFormula :: a -> EmpiricalFormula

-- | Smart constructor to make values of type 'EmpiricalFormula'.
mkEmpiricalFormula :: [(ElementSymbol, Int)] -> EmpiricalFormula
mkEmpiricalFormula l =
  let m = filterZero (fromList l)
  in EmpiricalFormula $ (`div` greatestCommonDenom m) <$> m

instance ToEmpiricalFormula ElementalComposition where
  toEmpiricalFormula (ElementalComposition m) =
    EmpiricalFormula $ (`div` greatestCommonDenom m) <$> m

instance ToEmpiricalFormula MolecularFormula where
  toEmpiricalFormula (MolecularFormula m) =
    EmpiricalFormula $ (`div` greatestCommonDenom m) <$> m

instance ToEmpiricalFormula CondensedFormula where
  toEmpiricalFormula = toEmpiricalFormula . toMolecularFormula

instance ToElementalComposition EmpiricalFormula where
  toElementalComposition (EmpiricalFormula a) = ElementalComposition a

instance Formula EmpiricalFormula where
   renderFormula f = foldMap renderFoldfunc
                     ((sortElementSymbolMap . getEmpiricalFormula) f)
   emptyFormula = mkEmpiricalFormula []

-- Helper function to find the greatest common denominator in a map.
greatestCommonDenom :: (Integral v) => Map k v -> v
greatestCommonDenom = foldr gcd 0
