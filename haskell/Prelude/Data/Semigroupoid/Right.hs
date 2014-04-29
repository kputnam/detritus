{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.Right where
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Disjoint
import Data.Coproduct

class (Semigroupoid m, Disjoint m) => Right m where
  right :: m a b -> m (c :+ a) (c :+ b)

instance Right (->) where
  right f (R a) = R (f a)
  right _ (L c) = L c
