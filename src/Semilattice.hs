module Semilattice where

class PartialOrd a where
  (<=*) :: a -> a -> Bool
  -- inspired by: http://hackage.haskell.org/package/lattices-2.0.1/docs/Algebra-PartialOrd.html
  comparable :: a -> a -> Bool
  comparable x y = (x <=* y) || (y <=* x)

class (Eq a, PartialOrd a, Show a) => Join a where
  (\/) :: a -> a -> a
