{-# LANGUAGE NoImplicitPrelude #-}

module Data.Order.Bound where
import Data.Order.Order

class MinBound a where
  minBound :: a

class MaxBound a where
  maxBound :: a

instance MinBound Ordering where
  minBound = LT

instance MaxBound Ordering where
  maxBound = GT
