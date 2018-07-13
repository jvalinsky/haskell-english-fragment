module Plural (Atomic, PluralEntity(Atom, Plural)) where

import Model
import Data.List (union)

data PluralEntity = Atom Entity | Plural [Entity] deriving (Eq, Show)

class (Eq s, Show s) => Atomic s where
    atoms :: [s]
    atom :: s -> Bool
    atom x = if x `elem` atoms then True else False

instance Atomic PluralEntity where
    atoms = [ Atom x | x <- entities ] 






    
