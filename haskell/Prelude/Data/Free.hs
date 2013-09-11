{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Functor.Free where
import Data.Functor.Functor
import Data.Functor.Apply
import Data.Functor.Applicative
import Data.Functor.Bind
import Data.Functor.Monad
import Data.Bool
import Data.Order.Eq
import Data.Order.Order

data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  f +$ (Pure a) = Pure (f a)
  f +$ (Free k) = Free ((f +$) +$ k)

instance Functor f => Apply (Free f) where
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Free k = Free ((f +$) +$ k)
  Free f <*> k      = Free ((<*> k) +$ f)

instance Functor f => Applicative (Free f) where
  pure = Pure

instance Functor f => Bind (Free f) where
  f =<< Pure a = f a
  f =<< Free k = Free ((f =<<) +$ k)
            -- = join (f +$ Free k)

  join (Pure a) = a
  join (Free a) = Free (join +$ a)

instance Functor f => Monad (Free f) where

instance (Eq a, Eq (f (Free f a))) => Eq (Free f a) where
  Pure a == Pure b = a == b
  Free f == Free g = f == g
  _ == _ = False

instance (Order a, Order (f (Free f a))) => Order (Free f a) where
  compare (Pure _) (Free _) = LT
  compare (Free _) (Pure _) = GT
  compare (Pure a) (Pure b) = compare a b
  compare (Free a) (Free b) = compare a b

lift :: Functor f => f a -> Free f a
lift a = Free (Pure +$ a)

hole :: a
hole = hole

retract :: Monad f => Free f a -> f a
retract = hole

iter :: Functor f => (f a -> a) -> Free f a -> a
iter _ (Pure a) = a
iter f (Free a) = f (iter f +$ a)
