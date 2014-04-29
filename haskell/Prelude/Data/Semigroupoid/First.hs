{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.First where
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Tensor
import Data.Product

class (Semigroupoid m, Tensor m) => First m where
  first :: m a b -> (a :* c) -> (b :* c)

instance First (->) where
  first f (a :* c) = f a :* c
