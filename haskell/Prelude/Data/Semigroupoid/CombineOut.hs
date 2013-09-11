{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.CombineOut where
import Data.Semigroupoid.Semigroupoid
import Data.Product

class Semigroupoid (~>) => CombineOut (~>) where
  (*-*) :: a ~> x -> b ~> x -> (a :* b) ~> x
