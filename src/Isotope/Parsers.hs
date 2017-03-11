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
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Data.List
import Data.Map (Map)
import Data.Monoid ((<>))

-- | Parses an element symbol string.
elementSymbol :: Parser ElementSymbol
elementSymbol = read <$> choice (try . string <$> elementSymbolStrList)
  where
    elementList = show <$> elementSymbolList
    reverseLengthSort x y = length y `compare` length x
    elementSymbolStrList = sortBy reverseLengthSort elementList

-- | Parses an sub-formula (i.e., \"C2\").
subFormula :: Parser (ElementSymbol, Int)
subFormula =
  (\sym num -> (sym, fromIntegral num)) <$> elementSymbol <*> option 1 L.integer

-- | Parses an elemental composition (i.e. \"C6H6\").
elementalComposition :: Parser ElementalComposition
elementalComposition = mkElementalComposition <$> many subFormula

-- | Parses a molecular formula (i.e. \"C6H6\").
molecularFormula :: Parser MolecularFormula
molecularFormula = mkMolecularFormula <$> many subFormula

-- | Parses a condensed formula, i.e., \"N(CH3)3\".
condensedFormula :: Parser CondensedFormula
condensedFormula =
  CondensedFormula <$> many (leftCondensedFormula <|> rightCondensedFormula)
  where
    subMolecularFormula :: Parser MolecularFormula
    subMolecularFormula = mkMolecularFormula . pure <$> subFormula
    leftCondensedFormula :: Parser (Either MolecularFormula (CondensedFormula, Int))
    leftCondensedFormula = Left <$> subMolecularFormula
    rightCondensedFormula :: Parser (Either MolecularFormula (CondensedFormula, Int))
    rightCondensedFormula = do
       _ <- char '('
       formula <- condensedFormula
       _ <- char ')'
       num <- option 1 L.integer
       return $ Right (formula, fromIntegral num)

-- | Parses a empirical formula (i.e. \"CH\").
empiricalFormula :: Parser EmpiricalFormula
empiricalFormula = mkEmpiricalFormula <$> many subFormula

-- Helper function for `ElementalComposition` quasiquoter
quoteElementalComposition :: String -> Q Exp
quoteElementalComposition s =
  case parse (condensedFormula <* eof) "" s of
    Left err -> fail $
      "Could not parse elemental formula!\n" <> parseErrorPretty err
    Right v  -> lift $ toElementalComposition v

-- Helper function for `MolecularFormula` quasiquoter
quoteMolecularFormula :: String -> Q Exp
quoteMolecularFormula s =
  case parse (condensedFormula <* eof) "" s of
    Left err -> fail $
      "Could not parse molecular formula!\n" <> parseErrorPretty err
    Right v  -> lift $ toMolecularFormula v

-- Helper function for `CondensedFormula` quasiquoter
quoteCondensedFormula :: String -> Q Exp
quoteCondensedFormula s =
  case parse (condensedFormula <* eof) "" s of
    Left err -> fail $
      "Could not parse condensed formula!\n" <> parseErrorPretty err
    Right v  -> lift v

-- Helper function for `EmpiricalFormula` quasiquoter
quoteEmpiricalFormula s =
  case parse (condensedFormula <* eof) "" s of
    Left err -> fail $
      "Could not parse empirical formula!\n" <> parseErrorPretty err
    Right v  -> lift $ toEmpiricalFormula v

-- | Quasiquoter for `ElementalComposition`
ele :: QuasiQuoter
ele = QuasiQuoter {
    quoteExp = quoteElementalComposition
  , quotePat  = notHandled "patterns" "elemental composition"
  , quoteType = notHandled "types" "elemental composition"
  , quoteDec  = notHandled "declarations" "elemental composition"
  }

-- | Quasiquoter for `MolecularFormula`
mol :: QuasiQuoter
mol = QuasiQuoter {
    quoteExp = quoteMolecularFormula
  , quotePat  = notHandled "patterns" "molecular formula"
  , quoteType = notHandled "types" "molecular formula"
  , quoteDec  = notHandled "declarations" "molecular formula"
  }

-- | Quasiquoter for `CondensedFormula`
con :: QuasiQuoter
con = QuasiQuoter {
    quoteExp = quoteCondensedFormula
  , quotePat  = notHandled "patterns" "condensed formula"
  , quoteType = notHandled "types" "condensed formula"
  , quoteDec  = notHandled "declarations" "condensed formula"
  }

-- | Quasiquoter for `EmpiricalFormula`
emp :: QuasiQuoter
emp = QuasiQuoter {
    quoteExp = quoteEmpiricalFormula
  , quotePat  = notHandled "patterns" "empirical formula"
  , quoteType = notHandled "types" "empirical formula"
  , quoteDec  = notHandled "declarations" "empirical formula"
  }

-- Helper function used in QuasiQuoters
notHandled :: String -> String -> a
notHandled feature quoterName =
  error $ feature <> " are not handled by the" <> quoterName <> "quasiquoter."

$(deriveLift ''ElementSymbol)

$(deriveLift ''ElementalComposition)

$(deriveLift ''MolecularFormula)

$(deriveLift ''CondensedFormula)

$(deriveLift ''EmpiricalFormula)

$(deriveLift ''Map)
