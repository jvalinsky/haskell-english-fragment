module Mass (Mass(..), toMass) where

import Model
import Plural

data Mass s = MatterOf s | Nothing | Everything

data MassEntity = Water | Gold | Metal | Wood

materalize :: PluralEntity -> Mass PluralEntity 
materalize x = undefined
