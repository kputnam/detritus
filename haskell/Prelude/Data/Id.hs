{-# LANGUAGE NoImplicitPrelude #-}

module Data.Id where
import Data.Semigroupoid.Semigroup
import Data.Semigroupoid.Monoid
import Data.Functor.Functor
import Data.Functor.Apply
import Data.Functor.Applicative
import Data.Functor.Bind
import Data.Functor.Monad
import Data.Functor.Extend
import Data.Functor.Comonad
import Data.Functor.Foldable
import Data.Functor.Foldable1
import Data.Order.Eq
import Data.Order.Order
import Data.Order.Bound

newtype Id a
  = Id a

instance Semigroup a => Semigroup (Id a) where
  Id a <> Id b = Id (a <> b)

instance Monoid a => Monoid (Id a) where
  identity = Id identity

instance Functor Id where
  f +$ Id a = Id (f a)

instance Apply Id where
  Id f <*> Id a = Id (f a)

instance Applicative Id where
  pure = Id

instance Bind Id where
  f =<< Id a = f a

instance Monad Id where

instance Extend Id where
  extend f a = Id (f a)

instance Comonad Id where
  extract (Id a) = a

instance Foldable Id where
  foldMap m (Id a) = m a

instance Foldable1 Id where
  foldMap1 m (Id a) = m a

instance Eq a => Eq (Id a) where
  Id a == Id b = a == b

instance Order a => Order (Id a) where
  compare (Id a) (Id b) = compare a b

instance MinBound a => MinBound (Id a) where
  minBound = Id minBound

instance MaxBound a => MaxBound (Id a) where
  maxBound = Id maxBound
