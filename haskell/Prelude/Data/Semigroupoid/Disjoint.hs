{-# LANGUAGE NoImplicitParams #-}
{-# LANGUAGE TypeOperators    #-}

module Data.Semigroupoid.Disjoint where
import Data.Semigroupoid.Semigroupoid
import Data.Coproduct

class Semigroupoid (~>) => Disjoint (~>) where
  (+++) :: a ~> b -> c ~> d -> (a :+ c) ~> (b :+ d)

instance Disjoint (->) where
  (+++) f _ (L a) = L (f a)
  (+++) _ g (R b) = R (g b)
