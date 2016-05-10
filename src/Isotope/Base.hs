{-|
Module      : Isotopic.Base
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
values of type 'Element'.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK hide #-}
module Isotope.Base (
    -- Type synonyms for masses
      IntegerMass
    , MonoisotopicMass
    , NominalMass
    , AverageMass
    , IsotopicMass
    -- Other type synonyms
    , ElementName
    , IsotopicAbundance
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
    -- 'ChemicalMass' type class
    , ChemicalMass(..)
    -- Molecular formulae
    , MolecularFormula(..)
    , (|+|)
    , (|-|)
    , (|*|)
    , Formula(..)
    -- Condensed formulae
    , CondensedFormula(..)
    , EmpiricalFormula(..)
    , ToEmpiricalFormula(..)
    ) where

import Prelude hiding      (lookup,filter)
import Data.Map            ( Map
                           , fromList
                           , foldMapWithKey
                           , unionWith
                           , filter
                           , mapWithKey
                           , lookup
                           , (!))
import Data.List           (elemIndex)
import Data.Maybe          (fromJust)

--------------------------------------------------------------------------------
-- Type synonyms for masses

-- | Integer mass for an isotope.
type IntegerMass       = MassNumber

-- | The exact mass of the most abundant isotope for an element or the sum of
-- the exact masses of the most abundant isotope of each element for a
-- molecular formula.
type MonoisotopicMass  = Double

-- | The integer mass of the most abundant isotope for an element or the sum of
-- integer mass of the most abundant isotope of each element for a chemical
-- formula.
type NominalMass       = Int

-- | The average mass of an element or molecular formula based on
-- naturally-occurring abundances.
type AverageMass       = Double

-- | The exact mass of an isotope.
type IsotopicMass      = Double

--------------------------------------------------------------------------------
-- Other type synonyms

-- | The name of an element.
type ElementName       = String

-- | The natural abundance of an isotope.
type IsotopicAbundance = Double

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
                     Rb | Sr | Y  | Zr | Nb | Mo | Tc | Ru | Rh | Pd | Ag | Cd |
                     In | Sn | Sb | Te | I  | Xe | Cs | Ba | La | Ce | Pr | Nd |
                     Pm | Sm | Eu | Gd | Tb | Dy | Ho | Er | Tm | Yb | Lu | Hf |
                     Ta | W  | Re | Os | Ir | Pt | Au | Hg | Tl | Pb | Bi | Th |
                     Pa | U  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | List containing all element symbols.
elementSymbolList :: [ElementSymbol]
elementSymbolList = [H .. U]

--------------------------------------------------------------------------------
-- Functions taking an 'Element' as input

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

-- | Monoistopic mass for an element.
elementMonoisotopicMass :: Element -> IsotopicMass
elementMonoisotopicMass = isotopicMass . elementMostAbundantIsotope

-- | Nominal mass for an element.
elementNominalMass :: Element -> MassNumber
elementNominalMass = massNumber . nucleons . elementMostAbundantIsotope

-- | Average mass of an element.
elementAverageMass :: Element -> IsotopicMass
elementAverageMass e = sum [isotopicMass x * isotopicAbundance x |
                            x <- isotopes' e]

-- Mass number for an isotope. Mass number is the number of protons plus the
-- number of neutrons.
massNumber :: Nucleons -> MassNumber
massNumber (protonNum, neutronNum) = protonNum + neutronNum
--------------------------------------------------------------------------------
-- 'elements' - a map containing isotopic data for each element.

-- | 'ElementSymbolMap' of the periodic table. All data on isotopic masses and
-- abundances is contained within this map.
elements :: Map ElementSymbol Element
elements = fromList
  [ (H,  Element 1  "hydrogen"     [ Isotope (1, 0)     1.00782503223  0.999885
                                   , Isotope (1, 1)     2.01410177812  0.000115 ])
  , (He, Element 2  "helium"       [ Isotope (2, 1)     3.0160293201   0.00000134
                                   , Isotope (2, 2)     4.00260325413  0.99999866 ])
  , (Li, Element 3  "lithium"      [ Isotope (3, 3)     6.0151228874   0.0759
                                   , Isotope (3, 4)     7.0160034366   0.9241 ])
  , (Be, Element 4  "beryllium"    [ Isotope (4, 5)     9.012183065    1 ])
  , (B,  Element 5  "boron"        [ Isotope (5, 5)     10.01293695    0.199
                                   , Isotope (5, 6)     11.00930536    0.801 ])
  , (C,  Element 6  "carbon"       [ Isotope (6, 6)     12.0000000     0.9893
                                   , Isotope (6, 7)     13.00335483507 0.0107 ])
  , (N,  Element 7  "nitrogen"     [ Isotope (7, 7)     14.00307400443 0.99636
                                   , Isotope (7, 8)     15.00010889888 0.00364 ])
  , (O,  Element 8  "oxygen"       [ Isotope (8, 8)     15.99491461957 0.99757
                                   , Isotope (8, 9)     16.99913175650 0.00038
                                   , Isotope (8, 10)    17.99915961286 0.00205 ])
  , (F,  Element 9  "fluorine"     [ Isotope (9, 10)    18.99840316273 1 ])
  , (Ne, Element 10 "neon"         [ Isotope (10, 10)   19.9924401762  0.9048
                                   , Isotope (10, 11)   20.993846685   0.0027
                                   , Isotope (10, 12)   21.991385114   0.0925 ])
  , (Na, Element 11 "sodium"       [ Isotope (11, 12)   22.9897692820  1 ])
  , (Mg, Element 12 "magnesium"    [ Isotope (12, 12)   23.985041697   0.7899
                                   , Isotope (12, 13)   24.985836976   0.1000
                                   , Isotope (12, 14)   25.982592968   0.1101 ])
  , (Al, Element 13 "aluminium"    [ Isotope (13, 14)   26.98153853    1 ])
  , (Si, Element 14 "silicon"      [ Isotope (14, 14)   27.97692653465 0.92223
                                   , Isotope (14, 15)   28.97649466490 0.04685
                                   , Isotope (14, 16)   29.973770136   0.03092 ])
  , (P,  Element 15 "phosphorous"  [ Isotope (15, 16)   30.97376199842 1 ])
  , (S,  Element 16 "sulfur"       [ Isotope (16, 16)   31.9720711744  0.9499
                                   , Isotope (16, 17)   32.9714589098  0.0075
                                   , Isotope (16, 18)   33.967867004   0.0425
                                   , Isotope (16, 20)   35.96708071    0.0001 ])
  , (Cl, Element 17 "chlorine"     [ Isotope (17, 18)   34.968852682   0.7576
                                   , Isotope (17, 20)   36.965902602   0.2424 ])
  , (Ar, Element 18 "argon"        [ Isotope (18, 18)   35.967545105   0.003336
                                   , Isotope (18, 20)   37.96273211    0.000629
                                   , Isotope (18, 22)   39.9623831237  0.996035 ])
  , (K,  Element 19 "potassium"    [ Isotope (19, 20)   38.9637064864  0.932581
                                   , Isotope (19, 21)   39.963998166   0.000117
                                   , Isotope (19, 22)   40.9618252579  0.067302 ])
  , (Ca, Element 20 "calcium"      [ Isotope (20, 20)   39.962590863   0.96941
                                   , Isotope (20, 22)   41.95861783    0.00647
                                   , Isotope (20, 23)   42.95876644    0.00135
                                   , Isotope (20, 24)   43.95548156    0.02086
                                   , Isotope (20, 26)   45.9536890     0.00004
                                   , Isotope (20, 28)   47.95252276    0.00187 ])
  , (Sc, Element 21 "scandium"     [ Isotope (21, 24)   44.95590828    1 ])
  , (Ti, Element 22 "titanium"     [ Isotope (22, 24)   45.95262772    0.0825
                                   , Isotope (22, 25)   46.95175879    0.0744
                                   , Isotope (22, 26)   47.94794198    0.7372
                                   , Isotope (22, 27)   48.94786568    0.0541
                                   , Isotope (22, 28)   49.94478689    0.0518 ])
  , (V,  Element 23 "vanadium"     [ Isotope (23, 27)   49.94715601    0.00250
                                   , Isotope (23, 28)   50.94395704    0.99750 ])
  , (Cr, Element 24 "chromium"     [ Isotope (24, 26)   49.94604183    0.04345
                                   , Isotope (24, 28)   51.94050623    0.83789
                                   , Isotope (24, 29)   52.94064815    0.09501
                                   , Isotope (24, 30)   53.93887916    0.02365 ])
  , (Mn, Element 25 "manganese"    [ Isotope (25, 30)   54.93804391    1 ])
  , (Fe, Element 26 "iron"         [ Isotope (26, 28)   53.93960899    0.05845
                                   , Isotope (26, 30)   55.93493633    0.91754
                                   , Isotope (26, 31)   56.93539284    0.02119
                                   , Isotope (26, 32)   57.93327443    0.00282 ])
  , (Co, Element 27 "cobalt"       [ Isotope (27, 32)   58.93319429    1 ])
  , (Ni, Element 28 "nickel"       [ Isotope (28, 30)   57.93534241    0.68077
                                   , Isotope (28, 32)   59.93078588    0.26223
                                   , Isotope (28, 33)   60.93105557    0.011399
                                   , Isotope (28, 34)   61.92834537    0.036346
                                   , Isotope (28, 36)   63.92796682    0.009255 ])
  , (Cu, Element 29 "copper"       [ Isotope (29, 34)   62.92959772    0.6915
                                   , Isotope (29, 36)   64.92778970    0.3085 ])
  , (Zn, Element 30 "zinc"         [ Isotope (30, 34)   63.92914201    0.4917
                                   , Isotope (30, 36)   65.92603381    0.2773
                                   , Isotope (30, 37)   66.92712775    0.0404
                                   , Isotope (30, 38)   67.92484455    0.1845
                                   , Isotope (30, 40)   69.9253192     0.0061 ])
  , (Ga, Element 31 "gallium"      [ Isotope (31, 38)   68.9255735     0.60108
                                   , Isotope (31, 40)   70.92470258    0.39892 ])
  , (Ge, Element 32 "germanium"    [ Isotope (32, 38)   69.92424875    0.2057
                                   , Isotope (32, 40)   71.922075826   0.2745
                                   , Isotope (32, 41)   72.923458956   0.0775
                                   , Isotope (32, 42)   73.921177761   0.3650
                                   , Isotope (32, 44)   75.921402726   0.0773 ])
  , (As, Element 33 "arsenic"      [ Isotope (33, 42)   74.92159457    1 ])
  , (Se, Element 34 "selenium"     [ Isotope (34, 40)   73.922475934   0.0089
                                   , Isotope (34, 42)   75.919213704   0.0937
                                   , Isotope (34, 43)   76.919914154   0.0763
                                   , Isotope (34, 44)   77.91730928    0.2377
                                   , Isotope (34, 46)   79.9165218     0.4961
                                   , Isotope (34, 48)   81.9166995     0.0873 ])
  , (Br, Element 35 "bromine"      [ Isotope (35, 44)   78.9183376     0.5069
                                   , Isotope (35, 46)   80.9162897     0.4931 ])
  , (Kr, Element 36 "krypton"      [ Isotope (36, 42)   77.92036494    0.00355
                                   , Isotope (36, 44)   79.91637808    0.02286
                                   , Isotope (36, 46)   81.91348273    0.11593
                                   , Isotope (36, 47)   82.91412716    0.11500
                                   , Isotope (36, 48)   83.9114977282  0.56987
                                   , Isotope (36, 50)   85.9106106269  0.17279 ])
  , (Rb, Element 37 "rubidium"     [ Isotope (37, 48)   84.9117897379  0.7217
                                   , Isotope (37, 50)   86.9091805310  0.2783 ])
  , (Sr, Element 38 "strontium"    [ Isotope (38, 46)   83.9134191     0.0056
                                   , Isotope (38, 48)   85.9092606     0.0986
                                   , Isotope (38, 49)   86.9088775     0.0700
                                   , Isotope (38, 50)   87.9056125     0.8258 ])
  , (Y,  Element 39 "yttrium"      [ Isotope (39, 50)   88.9058403     1 ])
  , (Zr, Element 40 "zirconium"    [ Isotope (40, 50)   89.9046977     0.5145
                                   , Isotope (40, 51)   90.9056396     0.1122
                                   , Isotope (40, 52)   91.9050347     0.1715
                                   , Isotope (40, 54)   93.9063108     0.1738
                                   , Isotope (40, 56)   95.9082714     0.0280 ])
  , (Nb, Element 41 "niobium"      [ Isotope (41, 52)   92.9063730     1 ])
  , (Mo, Element 42 "molybdenum"   [ Isotope (42, 50)   91.90680796    0.1453
                                   , Isotope (42, 52)   93.90508490    0.0915
                                   , Isotope (42, 53)   94.90583877    0.1584
                                   , Isotope (42, 54)   95.90467612    0.1667
                                   , Isotope (42, 55)   96.90601812    0.0960
                                   , Isotope (42, 56)   97.90540482    0.2439
                                   , Isotope (42, 58)   99.9074718     0.0982 ])
  , (Tc, Element 43 "technetium"   [ Isotope (43, 55)   97.9072124     0 ])
  , (Ru, Element 44 "ruthenium"    [ Isotope (44, 52)   95.90759025    0.0554
                                   , Isotope (44, 54)   97.9052868     0.0187
                                   , Isotope (44, 55)   98.9059341     0.1276
                                   , Isotope (44, 56)   99.9042143     0.1260
                                   , Isotope (44, 57)   100.9055769    0.1706
                                   , Isotope (44, 58)   101.9043441    0.3155
                                   , Isotope (44, 59)   103.9054275    0.1862 ])
  , (Rh, Element 45 "rhodium"      [ Isotope (45, 58)   102.9054980    1 ])
  , (Pd, Element 46 "palladium"    [ Isotope (46, 56)   101.9056022    0.0102
                                   , Isotope (46, 58)   103.9040305    0.1114
                                   , Isotope (46, 59)   104.9050796    0.2233
                                   , Isotope (46, 60)   105.9034804    0.2733
                                   , Isotope (46, 62)   107.9038916    0.2646
                                   , Isotope (46, 64)   109.90517220   0.1172 ])
  , (Ag, Element 47 "silver"       [ Isotope (47, 60)   106.9050916    0.51839
                                   , Isotope (47, 62)   108.9047553    0.48161 ])
  , (Cd, Element 48 "cadmium"      [ Isotope (48, 58)   105.9064599    0.0125
                                   , Isotope (48, 60)   107.9041834    0.0089
                                   , Isotope (48, 62)   109.90300661   0.1249
                                   , Isotope (48, 63)   110.90418287   0.1280
                                   , Isotope (48, 64)   111.90276287   0.2413
                                   , Isotope (48, 65)   112.90440813   0.1222
                                   , Isotope (48, 66)   113.90336509   0.2873
                                   , Isotope (48, 68)   115.90476315   0.0749 ])
  , (In, Element 49 "indium"       [ Isotope (49, 64)   112.90406184   0.0429
                                   , Isotope (49, 66)   114.903878776  0.9571 ])
  , (Sn, Element 50 "tin"          [ Isotope (50, 62)   111.90482387   0.0097
                                   , Isotope (50, 64)   113.9027827    0.0066
                                   , Isotope (50, 65)   114.903344699  0.0034
                                   , Isotope (50, 66)   115.90174280   0.1454
                                   , Isotope (50, 67)   116.90295398   0.0768
                                   , Isotope (50, 68)   117.90160657   0.2422
                                   , Isotope (50, 69)   118.90331117   0.0859
                                   , Isotope (50, 70)   119.90220163   0.3258
                                   , Isotope (50, 72)   121.9034438    0.0463
                                   , Isotope (50, 74)   123.9052766    0.0579 ])
  , (Sb, Element 51 "antimony"     [ Isotope (51, 70)   120.9038120    0.5721
                                   , Isotope (51, 72)   122.9042132    0.4279 ])
  , (Te, Element 52 "tellurium"    [ Isotope (52, 68)   119.9040593    0.0009
                                   , Isotope (52, 70)   121.9030435    0.0255
                                   , Isotope (52, 71)   122.9042698    0.0089
                                   , Isotope (52, 72)   123.9028171    0.0474
                                   , Isotope (52, 73)   124.9044299    0.0707
                                   , Isotope (52, 74)   125.9033109    0.1884
                                   , Isotope (52, 76)   127.90446128   0.3174
                                   , Isotope (52, 78)   129.906222748  0.3408 ])
  , (I,  Element 53 "iodine"       [ Isotope (53, 74)   126.9044719    1 ])
  , (Xe, Element 54 "xenon"        [ Isotope (54, 70)   123.9058920    0.000952
                                   , Isotope (54, 72)   125.9042983    0.000890
                                   , Isotope (54, 74)   127.9035310    0.019102
                                   , Isotope (54, 75)   128.9047808611 0.264006
                                   , Isotope (54, 76)   129.903509349  0.040710
                                   , Isotope (54, 77)   130.90508406   0.212324
                                   , Isotope (54, 78)   131.9041550856 0.269086
                                   , Isotope (54, 80)   133.90539466   0.104357
                                   , Isotope (54, 82)   135.907214484  0.088573 ])
  , (Cs, Element 55 "caesium"      [ Isotope (55, 78)   132.9054519610 1 ])
  , (Ba, Element 56 "barium"       [ Isotope (56, 74)   129.9063207    0.00106
                                   , Isotope (56, 76)   131.9050611    0.00101
                                   , Isotope (56, 78)   133.90450818   0.02417
                                   , Isotope (56, 79)   134.90568838   0.06592
                                   , Isotope (56, 80)   135.90457573   0.07854
                                   , Isotope (56, 81)   136.90582714   0.11232
                                   , Isotope (56, 82)   137.90524700   0.71698 ])
  , (La, Element 57 "lanthanum"    [ Isotope (57, 81)   137.9071149    0.0008881
                                   , Isotope (57, 83)   138.9063563    0.9991119 ])
  , (Ce, Element 58 "cerium"       [ Isotope (58, 78)   135.90712921   0.00185
                                   , Isotope (58, 80)   137.905991     0.00251
                                   , Isotope (58, 82)   139.9054431    0.88450
                                   , Isotope (58, 84)   141.9092504    0.11114 ])
  , (Pr, Element 59 "praseodymium" [ Isotope (59, 82)   140.9076576    1 ])
  , (Nd, Element 60 "neodymium"    [ Isotope (60, 82)   141.9077290    0.27152
                                   , Isotope (60, 83)   142.9098200    0.12174
                                   , Isotope (60, 84)   143.9100930    0.23798
                                   , Isotope (60, 85)   144.9125793    0.08293
                                   , Isotope (60, 86)   145.9131226    0.17189
                                   , Isotope (60, 88)   147.9168993    0.05756
                                   , Isotope (60, 90)   149.9209022    0.05638 ])
  , (Pm, Element 61 "promethium"   [ Isotope (61, 84)   144.9127559    0 ])
  , (Sm, Element 62 "samarium"     [ Isotope (62, 82)   143.9120065    0.0307
                                   , Isotope (62, 83)   146.9149044    0.1499
                                   , Isotope (62, 84)   147.9148292    0.1124
                                   , Isotope (62, 85)   148.9171921    0.1382
                                   , Isotope (62, 86)   149.9172829    0.0738
                                   , Isotope (62, 88)   151.9197397    0.2675
                                   , Isotope (62, 90)   153.9222169    0.2275 ])
  , (Eu, Element 63 "europium"     [ Isotope (63, 88)   150.9198578    0.4781
                                   , Isotope (63, 90)   152.9212380    0.5219 ])
  , (Gd, Element 64 "gadolinium"   [ Isotope (64, 88)   151.9197995    0.0020
                                   , Isotope (64, 90)   153.9208741    0.0218
                                   , Isotope (64, 91)   154.9226305    0.1480
                                   , Isotope (64, 92)   155.9221312    0.2047
                                   , Isotope (64, 93)   156.9239686    0.1565
                                   , Isotope (64, 94)   157.9241123    0.2484
                                   , Isotope (64, 96)   159.9270624    0.2186 ])
  , (Tb, Element 65 "terbium"      [ Isotope (65, 94)   158.9253547    1 ])
  , (Dy, Element 66 "dysprosium"   [ Isotope (66, 90)   155.9242847    0.00056
                                   , Isotope (66, 92)   157.9244159    0.00095
                                   , Isotope (66, 94)   159.9252046    0.02329
                                   , Isotope (66, 95)   160.9269405    0.18889
                                   , Isotope (66, 96)   161.9268056    0.25475
                                   , Isotope (66, 97)   162.9287383    0.24896
                                   , Isotope (66, 98)   163.9291819    0.28260 ])
  , (Ho, Element 67 "holmium"      [ Isotope (67, 98)   164.9303288    1 ])
  , (Er, Element 68 "erbium"       [ Isotope (68, 94)   161.9287884    0.00139
                                   , Isotope (68, 96)   163.9292088    0.01601
                                   , Isotope (68, 98)   165.9302995    0.33503
                                   , Isotope (68, 99)   166.9320546    0.22869
                                   , Isotope (68, 100)  167.9323767    0.26978
                                   , Isotope (68, 102)  169.9354702    0.14910 ])
  , (Tm, Element 69 "thulium"      [ Isotope (69, 100)  168.9342179    1 ])
  , (Yb, Element 70 "ytterbium"    [ Isotope (70, 98)   167.9338896    0.00123
                                   , Isotope (70, 100)  169.9347664    0.02982
                                   , Isotope (70, 101)  170.9363302    0.1409
                                   , Isotope (70, 102)  171.9363859    0.2168
                                   , Isotope (70, 103)  172.9382151    0.16103
                                   , Isotope (70, 104)  173.9388664    0.32026
                                   , Isotope (70, 106)  175.9425764    0.12996 ])
  , (Lu, Element 71 "lutetium"     [ Isotope (71, 104)  174.9407752    0.97401
                                   , Isotope (71, 105)  175.9426897    0.02599 ])
  , (Hf, Element 72 "hafnium"      [ Isotope (72, 102)  173.9400461    0.0016
                                   , Isotope (72, 104)  175.9414076    0.0526
                                   , Isotope (72, 105)  176.9432277    0.1860
                                   , Isotope (72, 106)  177.9437058    0.2728
                                   , Isotope (72, 107)  178.9458232    0.1362
                                   , Isotope (72, 108)  179.9465570    0.3508 ])
  , (Ta, Element 73 "tantalum"     [ Isotope (73, 107)  179.9474648    0.0001201
                                   , Isotope (73, 108)  180.9479958    0.9998799 ])
  , (W,  Element 74 "tungsten"     [ Isotope (74, 106)  179.9467108    0.0012
                                   , Isotope (74, 108)  181.94820394   0.2650
                                   , Isotope (74, 109)  182.95022275   0.1431
                                   , Isotope (74, 110)  183.95093092   0.3064
                                   , Isotope (74, 112)  185.9543628    0.2843 ])
  , (Re, Element 75 "rhenium"      [ Isotope (75, 110)  184.9529545    0.3740
                                   , Isotope (75, 112)  186.9557501    0.6260 ])
  , (Os, Element 76 "osmium"       [ Isotope (76, 108)  183.9524885    0.0002
                                   , Isotope (76, 110)  185.9538350    0.0159
                                   , Isotope (76, 111)  186.9557474    0.0196
                                   , Isotope (76, 112)  187.9558352    0.1324
                                   , Isotope (76, 113)  188.9581442    0.1615
                                   , Isotope (76, 114)  189.9584437    0.2626
                                   , Isotope (76, 116)  191.9614770    0.4078 ])
  , (Ir, Element 77 "iridium"      [ Isotope (77, 114)  190.9605893    0.373
                                   , Isotope (77, 116)  192.9629216    0.627 ])
  , (Pt, Element 78 "platinum"     [ Isotope (78, 112)  189.9599297    0.00012
                                   , Isotope (78, 114)  191.9610387    0.00782
                                   , Isotope (78, 116)  193.9626809    0.3286
                                   , Isotope (78, 117)  194.9647917    0.3378
                                   , Isotope (78, 118)  195.96495209   0.2521
                                   , Isotope (78, 120)  197.9678949    0.07356 ])
  , (Au, Element 79 "gold"         [ Isotope (79, 118)  196.96656879   1 ])
  , (Hg, Element 80 "mercury"      [ Isotope (80, 116)  195.9658326    0.0015
                                   , Isotope (80, 118)  197.96676860   0.0997
                                   , Isotope (80, 119)  198.96828064   0.1687
                                   , Isotope (80, 120)  199.96832659   0.2310
                                   , Isotope (80, 121)  200.97030284   0.1318
                                   , Isotope (80, 122)  201.97064340   0.2986
                                   , Isotope (80, 124)  203.97349398   0.0687 ])
  , (Tl, Element 81 "thallium"     [ Isotope (81, 122)  202.9723446    0.2952
                                   , Isotope (81, 124)  204.9744278    0.7048 ])
  , (Pb, Element 82 "lead"         [ Isotope (82, 122)  203.9730440    0.014
                                   , Isotope (82, 124)  205.9744657    0.241
                                   , Isotope (82, 125)  206.9758973    0.221
                                   , Isotope (82, 126)  207.9766525    0.524 ])
  , (Bi, Element 83 "bismuth"      [ Isotope (83, 126)  208.9803991    1 ])
  , (Th, Element 90 "thorium"      [ Isotope (90, 142)  232.0380558    1 ])
  , (Pa, Element 91 "protactinium" [ Isotope (91, 140)  231.0358842    1 ])
  , (U,  Element 92 "uranium"      [ Isotope (92, 142)  234.0409523    0.000054
                                   , Isotope (92, 143)  235.0439301    0.007204
                                   , Isotope (92, 146)  238.0507884    0.992742 ])
  ]

instance ChemicalMass ElementSymbol where
    getElementalComposition x = MolecularFormula . fromList $ [(x, 1)]

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
-- 'ChemicalMass' type class

-- | Class containing three methods; 'monoisotopicMass', 'nominalMass' and
-- 'averageMass'.
class ChemicalMass a where
     getElementalComposition :: a -> MolecularFormula
     monoisotopicMass        :: a -> MonoisotopicMass
     nominalMass             :: a -> NominalMass
     averageMass             :: a -> AverageMass
     monoisotopicMass = getFormulaSum elementMonoisotopicMass
     nominalMass      = getFormulaSum elementNominalMass
     averageMass      = getFormulaSum elementAverageMass
     {-# MINIMAL (getElementalComposition) #-}

-- Helper function for the calculating monoistopic masses, average mass and
-- nominal masses for molecular formulae.
getFormulaSum :: (Num a, ChemicalMass b) => (Element -> a) -> b -> a
getFormulaSum f m = sum $
    mapWithKey mapFunc (getMolecularFormula (getElementalComposition m))
  where mapFunc k v = (f . findElement) k * fromIntegral v

--------------------------------------------------------------------------------
-- Molecular formulae

-- | 'MolecularFormula' is a type synonym for @ElementSymbolMap Int@.
newtype MolecularFormula = MolecularFormula {
    getMolecularFormula :: Map ElementSymbol Int }
        deriving (Show, Read, Eq, Ord)

instance Monoid MolecularFormula where
   mempty = emptyFormula :: MolecularFormula
   mappend = (|+|)

-- | Infix operator for the addition of molecular formulae. (|+|) is mappend in
-- the monoid instance and the same fixity as (+).
(|+|) :: MolecularFormula ->  MolecularFormula ->  MolecularFormula
(|+|) =  combineMolecularFormulae (+)

-- | Infix operator for the subtraction of molecular formulae. Has the same
-- fixity as (-).
(|-|) :: MolecularFormula ->  MolecularFormula ->  MolecularFormula
(|-|) = combineMolecularFormulae (-)

-- | Infix operator for the multiplication of molecular formulae. Has the same
-- fixity as (*).
(|*|) :: Int -> MolecularFormula ->  MolecularFormula
n |*| m = MolecularFormula . filterZero $
              (fromIntegral n *) <$> getMolecularFormula m

infixl 6 |+|
infixl 7 |*|
infixl 6 |-|

-- The function unionWith adapted to work with 'ElementSymbolMap'.
combineMolecularFormulae :: (Int -> Int -> Int)
    -> MolecularFormula -> MolecularFormula -> MolecularFormula
combineMolecularFormulae f m1 m2 = MolecularFormula $
                                       filterZero $ unionWith f
                                                    (getMolecularFormula m1)
                                                    (getMolecularFormula m2)

-- Helper function to remove k v pairs where v == 0.
filterZero :: Map k Int -> Map k Int
filterZero = filter (/= 0)

instance ChemicalMass MolecularFormula where
    getElementalComposition = id

class FormulaHelperClass a  where
    renderFormula :: a -> String
    emptyFormula :: a

class FormulaHelperClass b => Formula a b where
    mkFormula :: a -> b


instance FormulaHelperClass MolecularFormula where
   renderFormula f = foldMapWithKey renderFoldfunc (getMolecularFormula f)
   emptyFormula = mkFormula ([] :: [(ElementSymbol, Int)])

instance Formula [(ElementSymbol, Int)] MolecularFormula where
  mkFormula = MolecularFormula . filterZero . fromList

-- | Produces a string with shorthand notation for a molecular formula.
renderFoldfunc :: (Eq b, Num b, Show a, Show b) => a -> b -> String
renderFoldfunc sym num = show sym ++ if num == 1
                                         then ""
                                         else show num
-- Use the Hill system for writing molecular formulas. C then H followed by
-- elements in alphabetical order.

--------------------------------------------------------------------------------
-- Condensed formulae

-- | `CondensedFormula` is a type synonym for condensed formulae.
newtype CondensedFormula = CondensedFormula {
    getCondensedFormula :: [Either MolecularFormula ([MolecularFormula], Int)] }
        deriving (Show, Read, Eq, Ord)

instance ChemicalMass CondensedFormula where
   getElementalComposition c = foldMap foldFunc (getCondensedFormula c)
       where foldFunc = \case
                         Left chemForm -> chemForm
                         Right (molForm, n) -> n |*| mconcat molForm

instance FormulaHelperClass CondensedFormula where
    renderFormula c = foldMap foldFunc (getCondensedFormula c)
        where foldFunc = \case
                          Left chemForm -> renderFormula chemForm
                          Right (chemFormList, n) ->
                              "(" ++ foldMap renderFormula chemFormList ++ ")" ++ formatNum n
                                  where formatNum n' = if n' == 1 then "" else show n'
    emptyFormula = CondensedFormula []

instance Formula [Either MolecularFormula ([MolecularFormula], Int)] CondensedFormula where
    mkFormula = CondensedFormula

--------------------------------------------------------------------------------
newtype EmpiricalFormula = EmpiricalFormula {
    getEmpiricalFormula :: Map ElementSymbol Int }
        deriving (Show, Read, Eq, Ord)

instance ChemicalMass EmpiricalFormula where
    getElementalComposition (EmpiricalFormula a) = MolecularFormula a

instance FormulaHelperClass EmpiricalFormula where
   renderFormula f = foldMapWithKey renderFoldfunc (getEmpiricalFormula f)
   emptyFormula = mkFormula ([] :: [(ElementSymbol, Int)])

instance Formula [(ElementSymbol, Int)] EmpiricalFormula where
  mkFormula = EmpiricalFormula . filterZero . fromList

class ToEmpiricalFormula a where
  toEmpiricalFormula :: a -> EmpiricalFormula

instance ToEmpiricalFormula MolecularFormula where
  toEmpiricalFormula (MolecularFormula m)
    | null m   = EmpiricalFormula m
    | otherwise = EmpiricalFormula $ (`div` greatestCommonDenom m) <$> m

instance ToEmpiricalFormula CondensedFormula where
  toEmpiricalFormula = toEmpiricalFormula . getElementalComposition

greatestCommonDenom :: (Integral v) => Map k v -> v
greatestCommonDenom = foldr gcd 0
