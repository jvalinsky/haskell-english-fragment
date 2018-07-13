module Mass (MassEntity(..), materalize) where

import Model
import Semilattice
import Plural (PluralEntity(..), PluralJoin(..), isum)

data MassEntity = MatterOf PluralEntity | Nothing | Everything

-- Homomorphism between individuals and mass terms
-- i.e. turn an individual into the mass associated with it
materalize :: PluralEntity -> MassEntity 
materalize (Atom x) = MatterOf (Atom x)
materalize (Plural x) = MatterOf (Plural x)

fusion :: MassEntity -> MassEntity -> MassEntity
fusion (MatterOf x) (MatterOf y) = MatterOf (x \/ y)

instance Join MassEntity where
    (\/) = fusion
    (<=-) = undefined
    (==-) = undefined
