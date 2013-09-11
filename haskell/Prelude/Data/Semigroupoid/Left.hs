{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.Left where
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Disjoint
import Data.Coproduct

class (Semigroupoid (~>), Disjoint (~>)) => Left (~>) where
  left :: a ~> b -> (a :+ c) ~> (b :+ c)

instance Left (->) where
  left f (L a) = L (f a)
  left _ (R c) = R c
