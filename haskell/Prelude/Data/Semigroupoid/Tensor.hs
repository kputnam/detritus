{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.Tensor where
import Data.Semigroupoid.Semigroupoid
import Data.Product

class Semigroupoid (~>) => Tensor (~>) where
  (***) :: a ~> b -> c ~> d -> (a :* c) ~> (b :* d)

instance Tensor (->) where
  (***) f g (a :* b) = f a :* g b
