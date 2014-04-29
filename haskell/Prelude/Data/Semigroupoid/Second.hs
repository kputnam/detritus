{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.Second where
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Tensor
import Data.Product

class (Semigroupoid m, Tensor m) => Second m where
  first :: m a b -> (c :* a) -> (c :* b)

instance Second (->) where
  first f (a :* c) = a :* f c

