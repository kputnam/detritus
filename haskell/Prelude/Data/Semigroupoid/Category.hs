{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Semigroupoid.Category where
import Data.Semigroupoid.Semigroupoid

class Semigroupoid m => Category m where
  id :: m a a

instance Category (->) where
  id a = a
