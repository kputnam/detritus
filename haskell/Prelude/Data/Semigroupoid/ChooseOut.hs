{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.ChooseOut where
import Data.Semigroupoid.Semigroupoid
import Data.Coproduct

class Semigroupoid (~>) => ChooseOut (~>) where
  (+-+) :: x ~> a -> x ~> b -> x ~> (a :+ b)
