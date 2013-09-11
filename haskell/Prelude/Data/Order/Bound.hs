{-# LANGUAGE NoImplicitPrelude #-}

module Data.Order.Bound where
import Data.Order.Order

class MinBound a where
  minBound :: a

class MaxBound a where
  maxBound :: a

class (MinBound a, MaxBound a) => Bound a where

instance MinBound Ordering where
  minBound = LT

instance MaxBound Ordering where
  maxBound = GT

instance Bound Ordering where
