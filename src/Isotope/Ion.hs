{-|
Module      : Isotope.Ion
Description : Provides support for ions.
Copyright   : Michael Thomas
License     : GPL-3
Maintainer  : Michael Thomas <Michaelt293@gmail.com>
Stability   : Experimental

This module allows the mass-to-charge ratio and polarity of ions to be
calculated.
-}
module Isotope.Ion where

import Isotope.Base
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), getSum, Sum(..))
import Data.Typeable (Typeable)
import Control.Exception.Safe (Exception, MonadThrow, throwM)

-- | The polarity of a charge. A charge can be either `Positive` or `Negative`.
data Polarity = Positive | Negative
  deriving (Show, Read, Eq, Ord)

-- | The mass-to-charge ratio of an ion.
newtype Mz = Mz { getMz :: Double }
  deriving (Show, Read, Eq, Ord)

-- | The `Ion` type class. This type class has two methods: `mz` and `polarity`.
class ToElementalComposition a => Ion a where
  mz :: MonadThrow m => a -> m Mz
  polarity :: MonadThrow m => a -> m Polarity
  mz a = fmap (\x -> Mz . abs $ monoisotopicMass' / x) charge'
   where
     monoisotopicMass' = getMonoisotopicMass $ monoisotopicMass a
     charge' = let charge'' = fromMaybe 0 (charge a)
               in if charge'' /= 0
                 then return (fromIntegral charge'')
                 else throwM IonHasChargeZero
  polarity a = polarity' $ fromMaybe 0 (charge a)
    where
      polarity' c
        | c > 0 = return Positive
        | c < 0 = return Negative
        | c == 0 = throwM IonHasChargeZero

data IonHasChargeZero = IonHasChargeZero deriving (Eq, Typeable)

instance Show IonHasChargeZero where
  show _ = "Ion has charge of 0"

instance Exception IonHasChargeZero

-- | Protonated represents a protonated ion.
newtype Protonated a = Protonated a deriving (Show, Read, Eq, Ord)

instance ToElementalComposition a => ToElementalComposition (Protonated a) where
  toElementalComposition (Protonated a) = toElementalComposition a |+|
                                          mkElementalComposition [(H, 1)]
  charge (Protonated a) = getSum <$> Just (Sum 1) <> (Sum <$> charge a)

instance ToElementalComposition a => Ion (Protonated a)

-- | `doublyProtonated` takes a type and returns a doubly `Protonated` ion.
doublyProtonated :: a -> Protonated (Protonated a)
doublyProtonated = Protonated . Protonated

-- | `Deprotonated` represents a deprotonated ion.
newtype Deprotonated a = Deprotonated a deriving (Show, Read, Eq, Ord)

instance ToElementalComposition a => ToElementalComposition (Deprotonated a) where
  toElementalComposition (Deprotonated a) = toElementalComposition a |-|
                                            mkElementalComposition [(H, 1)]
  charge (Deprotonated a) = getSum <$> Just (Sum (-1)) <> (Sum <$> charge a)

instance ToElementalComposition a => Ion (Deprotonated a)

-- | `doublyDeprotonated` takes a type and returns a doubly `Deprotonated` ion.
doublyDeprotonated :: a -> Deprotonated (Deprotonated a)
doublyDeprotonated = Deprotonated . Deprotonated
