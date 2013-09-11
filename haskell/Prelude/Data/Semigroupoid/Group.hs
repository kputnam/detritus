{-# LANGUAGE NoImplicitPrelude #-}

module Data.Semigroupoid.Group where
import Data.Semigroupoid.Monoid

class Monoid m => Group m where
  -- inverse x <> x = identity
  inverse :: m -> m
