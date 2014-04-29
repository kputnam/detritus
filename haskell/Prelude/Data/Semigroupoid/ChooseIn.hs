{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.ChooseIn where
import Data.Semigroupoid.Semigroupoid
import Data.Coproduct

class Semigroupoid m => ChooseIn m where
  (>+<) :: m a x -> m b x -> m (a :+ b) x

instance ChooseIn (->) where
  (>+<) f _ (L a) = f a
  (>+<) _ g (R b) = g b
