{-# LANGUAGE NoImplicitPrelude #-}

module Data.Order.Min where
import Data.Order.Order
import Data.Semigroupoid.Monoid
import Data.Semigroupoid.Semigroup

newtype Max a
  = Max a
  | Minimum

instance Order a => Semigroup (Max a) where
  Minimum <> a   = a
  a <> Minimum   = a
  Max a <> Max b = case compare a b of
    LT -> Max b
    GT -> Max a
    EQ -> Max a

instance Order a => Monoid (Max a) where
  identity = Minimum
