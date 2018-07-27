module Semilattice 
    (Join(..), AtomicJoin(..), difference) where

import Model
import Data.List 

class Join s where
    (\/) :: s -> s  -> s
    (<=-) :: s -> s -> Bool
    (==-) :: s -> s -> Bool

class (Join s) => AtomicJoin s where
    atom :: s -> Bool

-- Set difference for Lists
difference :: (Eq s) => [s] -> [s] -> [s]
difference [] y@(k:ks) = []
difference x@(j:js) [] = x
difference x@(j:js) y = if not (j `elem` y) then j:(difference js y) else difference js y
