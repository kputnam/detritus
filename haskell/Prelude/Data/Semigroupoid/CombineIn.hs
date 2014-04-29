{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.CombineIn where
import Data.Semigroupoid.Semigroupoid
import Data.Product

class Semigroupoid m => CombineIn m where
  (*-*) :: m x a -> m x b -> m x (a :* b)

instance CombineIn (->) where
  (*-*) f g x = f x :* g x
