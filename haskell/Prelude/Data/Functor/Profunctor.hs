{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Profunctor where
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Category

class Profunctor f where
  dmap :: (a' -> a) -> (b -> b') -> f a b -> f a' b'

lmap :: Profunctor f => (a' -> a) -> f a b -> f a' b
lmap = (`dmap` id)

rmap :: Profunctor f => (b -> b') -> f a b -> f a b'
rmap = (id `dmap`)

instance Profunctor (->) where
  dmap g h f = h . f . g
