{-|
Module      : Isotopic.Chemical
Description : Provides support for chemical and molecular formulae.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental

-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK hide #-}
module Isotope.Chemical (
  -- Type synonym
    ChemicalFormula
  , MolecularFormula
  -- Type classes
  , ToChemicalFormula(..)
  , FormulaMult(..)
  -- Functions
  , emptyChemicalFormula
  , renderChemicalFormula
  , renderMolecularFormula
  -- Operators
  , (|+|)
  , (|-|)
  ) where

import Isotope.Base
import Isotope.Periodic()
import Prelude hiding (filter)
import Data.Map hiding (map, (!))

-- | 'ChemicalFormula' is a type synonym for @ElementSymbolMap Int@.
type ChemicalFormula = ElementSymbolMap Int

-- | An empty chemical formula, i.e., a formula with no atoms. This is 'mempty'
-- in the 'Monoid' instance.
emptyChemicalFormula :: ChemicalFormula
emptyChemicalFormula = mkElementSymbolMap []

-- | Produces a string with shorthand notation for a chemical formula.
renderChemicalFormula :: (Eq a, Num a, Show a) => ElementSymbolMap a -> String
renderChemicalFormula f = foldMapWithKey foldfunc (getSymbolMap f)
    where foldfunc sym num = show sym ++ if num == 1
                                            then ""
                                            else show num

instance Monoid ChemicalFormula where
    mempty = emptyChemicalFormula
    mappend = (|+|)

-- | Class for types which can map to a chemical formula. This class has two
-- methods; getFormula and getMaybeFormula.
class ToChemicalFormula a where
    getFormula      :: a -> ChemicalFormula
    getMaybeFormula :: a -> Maybe ChemicalFormula

-- | Multiparameter type class for the |*| operator used to multiply chemical
-- formulas. (|*|) has the same fixity as (*).
class FormulaMult a b c | a b -> c where
    (|*|) :: a -> b -> c

-- | Infix operator for the addition of chemical formulae. (|+|) is mappend in
-- the monoid instance and the same fixity as (+).
(|+|) :: ChemicalFormula ->  ChemicalFormula ->  ChemicalFormula
(|+|) =  combineFormulae (+)

-- | Infix operator for the subtraction of chemical formulae. Has the same
-- fixity as (-).
(|-|) :: ChemicalFormula ->  ChemicalFormula ->  ChemicalFormula
(|-|) = combineFormulae (-)

-- | The function unionWith adapted to work with ElementSymbolMap. Filters out
-- key-value pairs with non-positive integers.
combineFormulae :: (Int -> Int -> Int) ->
                     ChemicalFormula ->  ChemicalFormula ->  ChemicalFormula
combineFormulae f m1 m2 = ElementSymbolMap $ unionWith f (getSymbolMap m1)
                                                         (getSymbolMap m2)

infixl 6 |+|
infixl 7 |*|
infixl 6 |-|

-- | Infix operator for the multiplication of chemical formulae. Has the same
-- fixity as (*).
instance FormulaMult ChemicalFormula Int ChemicalFormula where
    (|*|) = multChemicalFormula

-- | Infix operator for the multiplication of chemical formulae. Has the same
-- fixity as (*).
instance FormulaMult Int ChemicalFormula ChemicalFormula where
    (|*|) = flip multChemicalFormula

-- Helper function for the multiplication of chemical formulae.
multChemicalFormula :: ChemicalFormula -> Int ->  ChemicalFormula
multChemicalFormula m n = ElementSymbolMap $ (n *) <$> getSymbolMap m

-- | ChemicalFormula is an instance of Mass.
instance Mass ChemicalFormula where
    monoisotopicMass = getFormulaSum monoisotopicMass
    averageMass      = getFormulaSum averageMass
    nominalMass      = getFormulaSum nominalMass

-- Helper function for the calculating monoistopic masses, average mass and
-- nominal masses for chemical formulae.
getFormulaSum :: (Num a, Integral b) =>
                 (ElementSymbol -> a) -> ElementSymbolMap b -> a
getFormulaSum f m = sum $ mapWithKey mapFunc (getSymbolMap m)
    where mapFunc k v = f k * fromIntegral v

-- | `MolecularFormula` is a type synonym for molecular formulae.
type MolecularFormula = [Either ChemicalFormula ([ChemicalFormula], Int)]

instance Mass MolecularFormula where
    monoisotopicMass = monoisotopicMass . getFormula
    averageMass = averageMass . getFormula
    nominalMass = nominalMass . getFormula

instance ToChemicalFormula MolecularFormula where
    getFormula = foldMap (\case
        Left chemForm -> chemForm
        Right (molForm, n) -> mconcat molForm |*| n)
    getMaybeFormula x = Just (getFormula x)

-- | Takes a `MolecularFormula` as an argument an returns a formatted string,
-- i.e., in the form of \"N(CH3)3\".
renderMolecularFormula :: MolecularFormula -> String
renderMolecularFormula = foldMap (\case
    Left chemForm -> renderChemicalFormula chemForm
    Right (chemFormList, n) ->
        "(" ++ foldMap renderChemicalFormula chemFormList ++ ")" ++ formatNum n)
            where formatNum n' = if n' == 1 then "" else show n'
