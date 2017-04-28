module Isotope.Ion where

import Isotope.Base

data Polarity = Positive | Negative
  deriving (Show, Read, Eq, Ord)

newtype Mz = Mz { getMz :: Double }
  deriving (Show, Read, Eq, Ord)

class ToElementalComposition a => Ion a where
  mz :: a -> Mz
  polarity :: a -> Polarity
  mz a = Mz . abs $ monoisotopicMass' / charge'
   where
     monoisotopicMass' = getMonoisotopicMass $ monoisotopicMass a
     charge' = if charge a /= 0
                 then fromIntegral (charge a)
                 else error "An ion can't have a charge of 0!"
  polarity = polarity' . charge
    where
      polarity' c
        | c > 0 = Positive
        | c < 0 = Negative
        | c == 0 = error "An ion can't have a charge of 0!"

newtype Protonated a = Protonated a deriving (Show, Read, Eq, Ord)

instance ToElementalComposition a => ToElementalComposition (Protonated a) where
  toElementalComposition (Protonated a) = toElementalComposition a |+|
                                          mkElementalComposition [(H, 1)]
  charge (Protonated a) = 1 + charge a

instance ToElementalComposition a => Ion (Protonated a)

doublyProtonated = Protonated . Protonated

newtype Deprotonated a = Deprotonated a deriving (Show, Read, Eq, Ord)

instance ToElementalComposition a => ToElementalComposition (Deprotonated a) where
  toElementalComposition (Deprotonated a) = toElementalComposition a |-|
                                          mkElementalComposition [(H, 1)]
  charge (Deprotonated a) = -1 + charge a

instance ToElementalComposition a => Ion (Deprotonated a)

doublyDeprotonated = Deprotonated . Deprotonated
