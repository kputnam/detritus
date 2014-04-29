{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.Left where
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Disjoint
import Data.Coproduct

class (Semigroupoid m, Disjoint m) => Left m where
  left :: m a b -> m (a :+ c) (b :+ c)

instance Left (->) where
  left f (L a) = L (f a)
  left _ (R c) = R c
