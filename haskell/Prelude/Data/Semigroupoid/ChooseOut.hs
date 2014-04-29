{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.ChooseOut where
import Data.Semigroupoid.Semigroupoid
import Data.Coproduct

class Semigroupoid m => ChooseOut m where
  (+-+) :: m x a -> m x b -> m x (a :+ b)
