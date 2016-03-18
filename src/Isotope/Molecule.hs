{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Isotope.Molecule where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Data.String
import Control.Applicative
import Isotope.ElementIsotopes
import Isotope.Periodic
import Data.Map hiding (map)

type MolecularFormula = ElementSymbolMap Int

elemSym :: Parser ElementSymbol
elemSym = elemSym' <?> "element symbol"
    where elemSym' = do
                  upper <- upperChar
                  lower <- optional lowerChar
                  return $ case lower of
                                Nothing -> read [upper]
                                Just lower' -> read [upper, lower']

elemSymNum :: Parser MolecularFormula
elemSymNum = do
    sym <- elemSym
    num <- optional L.integer
    return $ case num of
                  Nothing -> mkElementSymbolMap [(sym, 1)]
                  Just num' -> mkElementSymbolMap [(sym, fromIntegral num')]

molecularFormula :: Parser MolecularFormula
molecularFormula = do
    formulas <- many elemSymNum
    return $ mconcat formulas

emptyFormula :: MolecularFormula
emptyFormula = mkElementSymbolMap []

renderFormula :: (Eq a, Num a, Show a) => ElementSymbolMap a -> String
renderFormula f = foldMapWithKey foldfunc (getSymbolMap f)
                      where foldfunc sym num = show sym ++ if num == 1
                                                              then ""
                                                              else show num

instance IsString MolecularFormula where
    fromString s = case parse (molecularFormula <* eof) "" s of
                        Left err -> error $ "Could not parse molecular formula: " ++ show err
                        Right v  -> v

instance Monoid MolecularFormula where
    mempty = emptyFormula
    mappend = (|+|)

class MolecularFormulae a where
    getFormula      :: a -> MolecularFormula
    getMaybeFormula :: a -> Maybe MolecularFormula

class FormulaMult a b c | a b -> c where
    (|*|) :: a -> b -> c

(|+|) :: Num a => ElementSymbolMap a -> ElementSymbolMap a -> ElementSymbolMap a
(|+|) = combineSymbolMaps (+)

(|-|) :: Num a => ElementSymbolMap a -> ElementSymbolMap a -> ElementSymbolMap a
(|-|) = combineSymbolMaps (-)

combineSymbolMaps :: (a -> a -> a) -> ElementSymbolMap a -> ElementSymbolMap a -> ElementSymbolMap a
combineSymbolMaps f m1 m2 = ElementSymbolMap $
                                unionWith f (getSymbolMap m1) (getSymbolMap m2)

infixl 6 |+|
infixl 7 |*|
infixl 6 |-|

instance FormulaMult MolecularFormula Int MolecularFormula where
    (|*|) = multMolecularFormula

instance FormulaMult Int MolecularFormula MolecularFormula where
    (|*|) = flip multMolecularFormula

multMolecularFormula :: Num a => ElementSymbolMap a -> a -> ElementSymbolMap a
multMolecularFormula m n = (n *) <$> m

monoisotopicMassFormula :: MolecularFormula -> IsotopicMass
monoisotopicMassFormula = getFormulaSum monoisotopicMass

averageMassFormula :: MolecularFormula -> IsotopicMass
averageMassFormula = getFormulaSum averageAtomicMass

nominalMassFormula :: MolecularFormula -> IntegerMass
nominalMassFormula = getFormulaSum nominalMass

getFormulaSum :: (Num a, Integral b) => (ElementSymbol -> a) -> ElementSymbolMap b -> a
getFormulaSum f m = sum $ foldFunc f symbolMap <$> keys symbolMap
    where symbolMap = getSymbolMap m
          foldFunc f' m' e = f' e * fromIntegral (m' ! e)
