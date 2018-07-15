module Plural (PluralEntity(Atom, Plural), PluralJoin(..), isum, list2PEnt) where

import Data.List
import Model
import Semilattice (Join(..), AtomicJoin(..), difference)

data PluralEntity = Atom Entity | Plural [Entity] deriving (Eq, Show)

isum :: PluralEntity -> PluralEntity -> PluralEntity
isum (Atom x) (Atom y) = if x /= y then Plural [x,y] else Atom x
isum (Atom x) (Plural y) = Plural (union [x] y)
isum (Plural x) (Atom y) = Plural (union x [y])
isum (Plural x) (Plural y) = Plural (union x y)

list2PEnt :: [Entity] -> PluralEntity
list2PEnt []  = Plural []
list2PEnt [x] = Atom x
list2PEnt y   = Plural y

instance Join PluralEntity where
    (\/) = isum
    (==-) (Atom x) (Atom y) = (x == y) 
    (==-) (Plural x) (Plural y) = if ((x `difference` y) == []) && 
        (length x >= length y) then True else False
    (==-) _ _ = False
    (<=-) x y = if (x \/ y) ==- y then True else False

instance AtomicJoin PluralEntity where
    atom (Atom x) = True
    atom (Plural x) = False

class (AtomicJoin s) => PluralJoin s where
    ipart :: s -> s -> Bool
   
instance PluralJoin PluralEntity where
    ipart x y = if (x `isum` y) /= y then False else True




    
