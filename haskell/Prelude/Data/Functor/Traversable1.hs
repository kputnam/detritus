{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Traversable1 where
import Data.Functor.Apply
import Data.Functor.Traversable

class Traversable t => Traversable1 t where
  traverse :: Apply f => (a -> f b) -> t a -> f (t b)
