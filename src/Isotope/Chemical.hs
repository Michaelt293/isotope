{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Isotope.Chemical where

import Prelude hiding (filter)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Data.String
import Isotope.Element
import Isotope.Periodic
import Data.Map hiding (map, (!))

-- | ChemicalFormula is a type synonym for ElementSymbolMap Int.
type ChemicalFormula = ElementSymbolMap Int

-- | Parses an element symbol string.
elementSymbol :: Parser ElementSymbol
elementSymbol = elementSymbol' <?> "element symbol"
    where elementSymbol' = do
                         upper <- upperChar
                         lower <- optional lowerChar
                         return $ case lower of
                                       Nothing -> read [upper]
                                       Just lower' -> read [upper, lower']

-- | Parses an sub-formula (i.e., "C2").
subFormula :: Parser ChemicalFormula
subFormula = do
    sym <- elementSymbol
    num <- optional L.integer
    return $ case num of
                  Nothing -> mkElementSymbolMap [(sym, 1)]
                  Just num' -> mkElementSymbolMap [(sym, fromIntegral num')]

-- | Parses a ChemicalFormula (i.e. "C6H6").
chemicalFormula :: Parser ChemicalFormula
chemicalFormula = do
    formulas <- many subFormula
    return $ mconcat formulas

-- | An empty chemical formula, i.e., a formula with no atoms. This is mempty in
-- in the Monoid instance.
emptyFormula :: ChemicalFormula
emptyFormula = mkElementSymbolMap []

-- | Produces a string with shorthand notation for a chemical formula.
renderFormula :: (Eq a, Num a, Show a) => ElementSymbolMap a -> String
renderFormula f = foldMapWithKey foldfunc (getSymbolMap f)
                      where foldfunc sym num = show sym ++ if num == 1
                                                              then ""
                                                              else show num

-- | IsString instance for ChemicalFormula. This allows the shorthand chemical
-- formula to be used directly in source code or in an interactive session.
-- E.g., "C6H6" :: ChemicalFormula
instance IsString ChemicalFormula where
    fromString s = case parse (chemicalFormula <* eof) "" s of
                        Left err -> error $ "Could not parse molecular formula: " ++ show err
                        Right v  -> v

-- | ChemicalFormula is an instance of Monoid.
instance Monoid ChemicalFormula where
    mempty = emptyFormula
    mappend = (|+|)

-- | Class for types which can map to a chemical formula. This class has two
-- methods; getFormula and getMaybeFormula.
class ChemicalFormulae a where
    getFormula      :: a -> ChemicalFormula
    getMaybeFormula :: a -> Maybe ChemicalFormula

-- | Multiparameter type class for the |*| operator used to multiply chemical
-- formulas.
class FormulaMult a b c | a b -> c where
    (|*|) :: a -> b -> c

-- | Infix operator for the addition of chemical formulae. (|+|) is mappend in
-- the monoid instance and the same fixity as (+).
(|+|) :: ChemicalFormula ->  ChemicalFormula ->  ChemicalFormula
(|+|) =  combineSymbolMaps (+)

-- | Infix operator for the subtraction of chemical formulae. Has the same
-- fixity as (-).
(|-|) :: ChemicalFormula ->  ChemicalFormula ->  ChemicalFormula
(|-|) = combineSymbolMaps (-)

-- | The function unionWith adapted to work with ElementSymbolMap. Filters out
-- key-value pairs with non-positive integers.
combineSymbolMaps :: (Int -> Int -> Int) -> ChemicalFormula ->  ChemicalFormula ->  ChemicalFormula
combineSymbolMaps f m1 m2 = ElementSymbolMap $
                                filter (<= 0) $ unionWith f (getSymbolMap m1) (getSymbolMap m2)

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

-- | Helper function for the multiplication of chemical formulae.
multChemicalFormula :: ChemicalFormula -> Int ->  ChemicalFormula
multChemicalFormula m n = ElementSymbolMap $
                              filter (<= 0) $ (n *) <$> getSymbolMap m

-- | ChemicalFormula is an instance of Mass.
instance Mass ChemicalFormula where
    monoisotopicMass = getFormulaSum monoisotopicMass
    averageMass      = getFormulaSum averageMass
    nominalMass      = getFormulaSum nominalMass

-- | Helper function for the calculating monoistopic masses, average mass and
-- nominal masses for chemical formulae.
getFormulaSum :: (Num a, Integral b) => (ElementSymbol -> a) -> ElementSymbolMap b -> a
getFormulaSum f m = sum $ mapWithKey mapFunc (getSymbolMap m)
                        where mapFunc key val = f key * fromIntegral val
