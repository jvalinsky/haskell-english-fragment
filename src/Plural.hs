module Plural (Atomic, PluralEntity(Atom, Plural)) where

import Data.List
import Model
import Semilattice (Join(..), AtomicJoin(..), difference)

data PluralEntity = Atom Entity | Plural [Entity] deriving (Eq, Show)

isum :: PluralEntity -> PluralEntity -> PluralEntity
isum (Atom x) (Atom y) = if x /= y then Plural [x,y] else Atom x
isum (Atom x) (Plural y) = Plural (union [x] y)
isum (Plural x) (Atom y) = Plural (union x [y])
isum (Plural x) (Plural y) = Plural (union x y)


-- A semilattice imposes a partial order
-- can't compare objects that are on different levels of the
-- hierarchy and one is not an i-part of the other
-- If the semilattice is atomic then,
-- for every b /= 0, then there exists an an atom a s.t. a <= b
comparable :: PluralEntity -> PluralEntity -> Bool
comparable (Atom x) (Atom y) = True
comparable (Plural x) (Atom y) = if y `elem` x then True else False
comparable (Atom x) (Plural y) = if x `elem` y then True else False
comparable (Plural x) (Plural y) =  

instance Join PluralEntity where
    (\/) = isum
    (==-) = (==) 
    (<=-) x y = if x \/ y ==- y then True else False

instance AtomicJoin PluralEntity where
    atom (Atom x) = True
    atom (Plural x) = False

class (AtomicJoin s) => PluralJoin s where
    ipart :: s -> s -> Bool
   
instance PluralJoin PluralEntity where
    ipart x y = if (x `isum` y) /= y then False else True




    
