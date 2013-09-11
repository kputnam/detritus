{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.Right where
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Disjoint
import Data.Coproduct

class (Semigroupoid (~>), Disjoint (~>)) => Right (~>) where
  right :: a ~> b -> (c :+ a) ~> (c :+ b)

instance Right (->) where
  right f (R a) = R (f a)
  right _ (L c) = L c
