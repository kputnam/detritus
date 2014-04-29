{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.Tensor where
import Data.Semigroupoid.Semigroupoid
import Data.Product

class Semigroupoid m => Tensor m where
  (***) :: m a b -> m c d -> m (a :* c) (b :* d)

instance Tensor (->) where
  (***) f g (a :* b) = f a :* g b
