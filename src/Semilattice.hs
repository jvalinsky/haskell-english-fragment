module Semilattice where

class (Eq a, Show a) => Join a where
  (\/) :: a -> a -> a
  --(<=)  :: a -> a -> Bool
