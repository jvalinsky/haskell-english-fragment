module Mass (MassEntity(..), materalize, MassJoin(..)) where

import Model
import Semilattice
import Plural

data MassEntity = MatterOf String | Nothing | Everything

-- Homomorphism between individuals and mass terms
-- i.e. turn an individual into the mass associated with it
materalize :: PluralEntity -> Mass PluralEntity 
materalize (Atom x) = MatterOf (Atom x)
materalize (Plural x) = MatterOf (Plural x)

class (Join s) => MassJoin s where
    fusion :: s -> s -> s

instance MassJoin MassEntity where
    fusion = undefined
