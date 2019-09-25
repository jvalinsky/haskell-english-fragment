{-# LANGUAGE DataKinds, TypeFamilies, TypeOperator #-}
{-# LANGUAGE UndecidableInstances #-}

module Semilattice where

class Meet a where
  (/\) :: a -> a -> a

class Join a where
  (\/) :: a -> a -> a

