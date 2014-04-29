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
--($$) :: (a -> a') -> (b -> b') -> a :+ b -> a' :+ b'
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

newtype SumT e m a
  = SumT { runSumT :: m (e :+ a) }
  -- SumT    :: m (e :+ a) -> SumT e m a
  -- runSumT :: SumT e m a -> m (e :+ a)

instance MonadTrans (SumT e) where
  liftM m = SumT (pure . pure =<< m)

instance MonadTransform (SumT e) where
  transform f = SumT . f . runSumT
