{-# LANGUAGE NoImplicitPrelude #-}

module Data.Order.Order where
import Data.Semigroupoid.Semigroup
import Data.Semigroupoid.Monoid
import Data.Order.Eq
import Data.Bool

data Ordering
  = LT
  | GT
  | EQ

instance Semigroup Ordering where
  LT <> _ = LT
  GT <> _ = GT
  EQ <> a = a

instance Monoid Ordering where
  identity = EQ

infix 4 <
infix 4 <=
infix 4 >
infix 4 >=

class Eq a => Order a where
  compare :: a -> a -> Ordering

  greater :: a -> a -> a
  greater a b = case compare a b of LT -> b; _ -> a

  lesser  :: a -> a -> a
  lesser  a b = case compare a b of GT -> b; _ -> a

  (<), (>), (<=), (>=) :: a -> a -> Bool
  a < b = case compare a b of LT -> True; _ -> False
  a > b = case compare a b of GT -> True; _ -> False
  a <= b = case compare a b of GT -> False; _ -> True
  a >= b = case compare a b of LT -> False; _ -> True

instance Order Bool where
  compare True  False = GT
  compare True  True  = EQ
  compare False True  = LT
  compare False False = EQ
