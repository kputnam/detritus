{-# LANGUAGE NoImplicitPrelude #-}

module Data.Order.Min where
import Data.Order.Order
import Data.Semigroupoid.Monoid
import Data.Semigroupoid.Semigroup

data Min a
  = Min a
  | Maximum

instance Order a => Semigroup (Min a) where
  Maximum <> a   = a
  a <> Maximum   = a
  Min a <> Min b = case compare a b of
    LT -> Min a
    GT -> Min b
    EQ -> Min b

instance Order a => Monoid (Min a) where
  identity = Maximum
