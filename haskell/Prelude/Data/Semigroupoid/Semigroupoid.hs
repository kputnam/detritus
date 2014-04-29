{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.Semigroupoid where

class Semigroupoid m where
  (.) :: m b c -> m a b -> m a c

instance Semigroupoid (->) where
  (.) f g x = f (g x)
