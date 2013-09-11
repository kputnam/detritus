{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Coproduct where
import Data.Semigroupoid.Semigroupoid
import Data.Functor.Functor
import Data.Functor.Bifunctor
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Functor.Applicative
import Data.Functor.Monad
import Data.Functor.MonadTrans
import Data.Functor.MonadTransform
import Data.Order.Eq
import Data.Bool

infix 6 :+

data a :+ b
  = L a
  | R b

-- Equivalent to Disjoint (->)
instance Bifunctor (:+) where
  ($$) f _ (L a) = L (f a)
  ($$) _ g (R b) = R (g b)

instance (Eq a, Eq b) => Eq (a :+ b) where
  L a == L a' = a == a'
  R b == R b' = b == b'
  _ == _      = False

instance Functor ((:+) c) where
  _ +$ (L c) = L c
  f +$ (R a) = R (f a)

instance Apply ((:+) c) where
  L c <*> _   = L c
  R _ <*> L c = L c
  R f <*> R a = R (f a)

instance Applicative ((:+) c) where
  pure = R

instance Bind ((:+) c) where
  _ =<< L c = L c
  f =<< R a = f a

instance Monad ((:+) a) where

newtype SumT c m a
  = SumT { runSumT :: m (c :+ a) }

instance MonadTrans (SumT c) where
  liftM m = SumT (pure . pure =<< m)

instance MonadTransform (SumT c) where
  transform f = SumT . f . runSumT
