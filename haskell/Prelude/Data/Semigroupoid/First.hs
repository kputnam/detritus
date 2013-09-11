{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.First where
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Tensor
import Data.Product

class (Semigroupoid (~>), Tensor (~>)) => First (~>) where
  first :: a ~> b -> (a :* c) -> (b :* c)

instance First (->) where
  first f (a :* c) = f a :* c
