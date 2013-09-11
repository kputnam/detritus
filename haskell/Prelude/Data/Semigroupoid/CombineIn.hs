{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.CombineIn where
import Data.Semigroupoid.Semigroupoid
import Data.Product

class Semigroupoid (~>) => CombineIn (~>) where
  (*-*) :: x ~> a -> x ~> b -> x ~> (a :* b)

instance CombineIn (->) where
  (*-*) f g x = f x :* g x
