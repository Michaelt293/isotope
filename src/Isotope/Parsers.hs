{-|
Module      : Isotopic.Parsers
Description : Parsers for chemical and molecular formulae.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental

This module provides parsers for element symbols as well chemical and molecular
formulae. In addition, instances of `IsString` are provided.
-}
{-# LANGUAGE FlexibleInstances #-}
module Isotope.Parsers (
    -- * Parsers
      elementSymbol
    , subFormula
    , chemicalFormula
    , molecularFormula
    ) where

import Isotope.Base
import Isotope.Chemical
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Data.String
import Data.List hiding (filter)

-- | Parses an element symbol string.
elementSymbol :: Parser ElementSymbol
elementSymbol = read <$> choice (try . string <$> elementSymbolStrList)
    where elementList = show <$> elementSymbolList
          reverseLengthSort x y = length y `compare` length x
          elementSymbolStrList = sortBy reverseLengthSort elementList

-- | Parses an sub-formula (i.e., \"C2\").
subFormula :: Parser ChemicalFormula
subFormula = do
    sym <- elementSymbol
    num <- optional L.integer
    return $ case num of
                  Nothing -> mkElementSymbolMap [(sym, 1)]
                  Just num' -> mkElementSymbolMap [(sym, fromIntegral num')]

-- | Parses a chemical formula (i.e. \"C6H6O\").
chemicalFormula :: Parser ChemicalFormula
chemicalFormula = do
    formulas <- many subFormula
    return $ mconcat formulas

instance IsString ChemicalFormula where
    fromString s =
      case parse (chemicalFormula <* eof) "" s of
           Left err -> error $ "Could not parse chemical formula: " ++ show err
           Right v  -> v

-- Helper function. Parses parenthesed sections in molecular formulae, i.e.,
-- the \"(CH3)3\" section of \"N(CH3)3\".
parenFormula :: Parser (Either ChemicalFormula ([ChemicalFormula], Int))
parenFormula = do
   _ <- char '('
   formula <- some subFormula
   _ <- char ')'
   num <- optional L.integer
   return $ Right $ case num of
                         Nothing -> (formula, 1)
                         Just num' -> (formula, fromIntegral num')

-- Helper function. Parses non-parenthesed sections in molecular formulae, i.e.,
-- the \"N\" section of \"N(CH3)3\".
leftChemicalFormula :: Parser (Either ChemicalFormula ([ChemicalFormula], Int))
leftChemicalFormula = do
   formula <- subFormula
   return $ Left formula

-- | Parsers a molecular formula, i.e., \"N(CH3)3\".
molecularFormula :: Parser MolecularFormula
molecularFormula = many (leftChemicalFormula <|> parenFormula)

instance IsString MolecularFormula where
   fromString s =
     case parse (molecularFormula <* eof) "" s of
          Left err -> error $ "Could not parse molecular formula: " ++ show err
          Right v  -> v
