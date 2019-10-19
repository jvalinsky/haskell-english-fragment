module Semilattice where

-- inspired by: http://hackage.haskell.org/package/lattices-2.0.1/docs/Algebra-PartialOrd.html
class PartialOrd a where
  (<=*) :: a -> a -> Bool
  comparable :: a -> a -> Bool
  comparable x y = (x <=* y) || (y <=* x)

class (Eq a, PartialOrd a, Show a) => Join a where
  (\/) :: a -> a -> a
