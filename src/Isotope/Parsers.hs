{-|
Module      : Isotope.Parsers
Description : Parsers for chemical and condensed formulae.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental

This module provides parsers for element symbols and elemental composition as
well molecular, condensed and empirical formulae. In addition, quasiquoters are
provided.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
module Isotope.Parsers (
    -- * Parsers
      elementSymbol
    , subFormula
    , elementalComposition
    , molecularFormula
    , condensedFormula
    , empiricalFormula
    , ele
    , mol
    , con
    , emp
    ) where

import Isotope.Base
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Data.String
import Data.List hiding (filter)
import Data.Map (Map)

-- | Parses an element symbol string.
elementSymbol :: Parser ElementSymbol
elementSymbol = read <$> choice (try . string <$> elementSymbolStrList)
    where elementList = show <$> elementSymbolList
          reverseLengthSort x y = length y `compare` length x
          elementSymbolStrList = sortBy reverseLengthSort elementList

-- | Parses an sub-formula (i.e., \"C2\").
subFormula :: Parser (ElementSymbol, Int)
subFormula = do
    sym <- elementSymbol
    num <- optional L.integer
    return $ case num of
                  Nothing   -> (sym, 1)
                  Just num' -> (sym, fromIntegral num')

-- | Parses an elemental composition (i.e. \"C6H6\").
elementalComposition :: Parser ElementalComposition
elementalComposition = mkElementalComposition <$> many subFormula

-- | Parses an sub-molecular-formula (i.e., \"C2\").
subMolecularFormula :: Parser MolecularFormula
subMolecularFormula = do
    form <- subFormula
    return $ mkMolecularFormula [form]

-- | Parses a molecular formula (i.e. \"C6H6\").
molecularFormula :: Parser MolecularFormula
molecularFormula = mkMolecularFormula <$> many subFormula

-- Helper function. Parses parenthesed sections in condensed formulae, i.e.,
-- the \"(CH3)3\" section of \"N(CH3)3\".
rightCondensedFormula :: Parser (Either MolecularFormula ([MolecularFormula], Int))
rightCondensedFormula = do
   _ <- char '('
   formula <- some subMolecularFormula
   _ <- char ')'
   num <- optional L.integer
   return $ Right $ case num of
                         Nothing   -> (formula, 1)
                         Just num' -> (formula, fromIntegral num')

-- Helper function. Parses non-parenthesed sections in condensed formulae, i.e.,
-- the \"N\" section of \"N(CH3)3\".
leftCondensedFormula :: Parser (Either MolecularFormula ([MolecularFormula], Int))
leftCondensedFormula = Left <$> subMolecularFormula

-- | Parses a condensed formula, i.e., \"N(CH3)3\".
condensedFormula :: Parser CondensedFormula
condensedFormula = do
  result <- many (leftCondensedFormula <|> rightCondensedFormula)
  return $ CondensedFormula result

-- | Parses a empirical formula (i.e. \"CH\").
empiricalFormula :: Parser EmpiricalFormula
empiricalFormula = mkEmpiricalFormula <$> many subFormula

-- Helper function for `ElementalComposition` quasiquoter
quoteElementalComposition s =
  case parse (condensedFormula <* eof) "" s of
    Left err -> error $ "Could not parse formula: " ++ show err
    Right v  -> lift $ toElementalComposition v

-- Helper function for `MolecularFormula` quasiquoter
quoteMolecularFormula s =
  case parse (condensedFormula <* eof) "" s of
    Left err -> fail $ "Could not parse formula: " ++ show err
    Right v  -> lift $ toMolecularFormula v

-- Helper function for `CondensedFormula` quasiquoter
quoteCondensedFormula s =
  case parse (condensedFormula <* eof) "" s of
    Left err -> error $ "Could not parse formula: " ++ show err
    Right v  -> lift v

-- Helper function for `EmpiricalFormula` quasiquoter
quoteEmpiricalFormula s =
  case parse (condensedFormula <* eof) "" s of
    Left err -> fail $ "Could not parse formula: " ++ show err
    Right v  -> lift $ toEmpiricalFormula v

-- | Quasiquoter for `ElementalComposition`
ele :: QuasiQuoter
ele = QuasiQuoter
    { quoteExp = quoteElementalComposition }

-- | Quasiquoter for `MolecularFormula`
mol :: QuasiQuoter
mol = QuasiQuoter
    { quoteExp = quoteMolecularFormula }

-- | Quasiquoter for `CondensedFormula`
con :: QuasiQuoter
con = QuasiQuoter
    { quoteExp = quoteCondensedFormula }

-- | Quasiquoter for `EmpiricalFormula`
emp :: QuasiQuoter
emp = QuasiQuoter
    { quoteExp = quoteEmpiricalFormula }

$(deriveLift ''ElementSymbol)

$(deriveLift ''ElementalComposition)

$(deriveLift ''MolecularFormula)

$(deriveLift ''CondensedFormula)

$(deriveLift ''EmpiricalFormula)

$(deriveLift ''Map)
