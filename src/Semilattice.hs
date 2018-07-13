module Semilattice 
    (Join(..), AtomicJoin(..), difference) where

import Model
import Data.List 

class Join s where
    (\/) :: s -> s  -> s
    cmp :: s -> s -> Maybe Ordering
    (<=-) :: s -> s -> Maybe Bool
    (==-) :: s -> s -> Bool
    cmp x y | (x <=- y) == Nothing = Nothing 
            | (x <=- y) == Just True = Just LT
            | (x <=- y) == Just False = Just GT
            | (x ==- y) == True = Just EQ

class (Join s) => AtomicJoin s where
    atom :: s -> Bool

difference :: (Eq s) => [s] -> [s] -> [s]
difference [] y@(k:ks) = []
difference x@(j:js) [] = x
difference x@(j:js) y = if not (j `elem` y) then j:(difference js y) else difference js y



