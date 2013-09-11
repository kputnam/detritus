{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.ChooseIn where
import Data.Semigroupoid.Semigroupoid
import Data.Coproduct

class Semigroupoid (~>) => ChooseIn (~>) where
  (>+<) :: a ~> x -> b ~> x -> (a :+ b) ~> x

instance ChooseIn (->) where
  (>+<) f _ (L a) = f a
  (>+<) _ g (R b) = g b
