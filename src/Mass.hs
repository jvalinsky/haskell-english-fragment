module Mass (MassEntity(..), materalize) where

import Model
import Semilattice
import Plural (PluralEntity(..), PluralJoin(..), isum)

-- MassEntity can be the mass corresponding to an individual or 
-- a lump of matter with an index and an amount in kilograms
data MassT = Water | Metal | Dirt deriving (Eq, Enum, Show)
data MassEntity = MatterOf PluralEntity | Mass MassT Int Float | Nothing | Everything

-- Homomorphism between individuals and mass terms
-- i.e. turn an individual into the mass associated with it
materalize :: PluralEntity -> MassEntity 
materalize (Atom x) = MatterOf (Atom x)
materalize (Plural x) = MatterOf (Plural x)

fusion :: MassEntity -> MassEntity -> MassEntity
fusion (MatterOf x) (MatterOf y) = MatterOf (x \/ y)
fusion (Mass t1 r1 x) (Mass t2 r2 y) | (t1 == t2) && (r1 /= r2) = 

instance Join MassEntity where
    (\/) = fusion
    (<=-) = undefined
    (==-) = undefined

