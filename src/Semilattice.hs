module Semilattice 
    (Join) where

import Model
import Plural
import Data.List 

class Join s where
    (\/) :: s -> s  -> s

class (Atomic s, Join s) => AtomicJoin s

class (Atomic s, Join s) => PluralT s where
    ipart :: s -> s -> Bool


sum :: PluralEntity -> PluralEntity -> PluralEntity
sum (Atom x) (Atom y) = if x /= y then Plural [x,y] else Atom x
sum (Atom x) (Plural y) = Plural (union [x] y)
sum (Plural x) (Atom y) = Plural (union x [y])
sum (Plural x) (Plural y) = Plural (union x y)

instance Join PluralEntity where
    (\/) = Semilattice.sum

instance PluralT PluralEntity where
    ipart (Atom x) (Atom y) = False
    ipart (Atom x) (Plural y) = if x `elem` y then True else False
    ipart (Plural x) (Atom y) = False
    ipart (Plural x) (Plural y) | length x >= length y = False
                                | length [ k | k <- x, k `elem` y ] == length x = True

