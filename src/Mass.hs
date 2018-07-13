module Mass where

import Model
import Plural

data Mass s = MatterOf s | Nothing | Everything

data MassEntity = Water | Gold | Metal | Wood

-- Homomorphism between indivudals and mass terms
materalize :: PluralEntity -> Mass PluralEntity 
materalize (Atom x) = MatterOf (Atom x)
materalize (Plural x) = MatterOf (Plural x)

class MassT s where
    fusion :: s -> s -> s

