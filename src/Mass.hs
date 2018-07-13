module Mass (Mass(..), toMass) where

import Model
import Plural

data Mass s = MatterOf s | Nothing | Everything

data MassEntity = Water | Gold | Metal | Wood

materalize :: PluralEntity -> Mass PluralEntity 
materalize (Atom x) = MatterOf (Atom x)
materalize (Plural x) = MatterOf (Plural x)

class MassT s where
    fusion :: s -> s -> s

