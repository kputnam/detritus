{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.CombineOut where
import Data.Semigroupoid.Semigroupoid
import Data.Product

class Semigroupoid m => CombineOut m where
  (*-*) :: m a x -> m b x -> m (a :* b) x
