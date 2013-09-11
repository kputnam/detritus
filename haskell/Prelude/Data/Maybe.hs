{-# LANGUAGE NoImplicitPrelude #-}

module Data.Maybe where
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Semigroup
import Data.Semigroupoid.Monoid
import Data.Functor.Functor
import Data.Functor.Apply
import Data.Functor.Applicative
import Data.Functor.Bind
import Data.Functor.Monad
import Data.Functor.Alt
import Data.Functor.Plus
import Data.Functor.MonadAlt
import Data.Functor.MonadPlus
import Data.Functor.MonadTrans
import Data.Functor.MonadTransform
import Data.Functor.Foldable
import Data.Bool
import Data.Order.Eq
import Data.Order.Order
import Data.Order.Bound

data Maybe a
  = Just a
  | Nothing

fold :: (a -> r) -> r -> Maybe a -> r
fold f _ (Just a) = f a
fold _ z Nothing  = z

instance Semigroup (Maybe a) where
  a <> b = fold Just b a

instance Monoid (Maybe a) where
  identity = Nothing

instance Functor Maybe where
  f +$ a = fold (Just . f) Nothing a

instance Apply Maybe where
  f <*> a = fold (+$ a) Nothing f

instance Applicative Maybe where
  pure = Just

instance Bind Maybe where
  f =<< a = fold f Nothing a

instance Monad Maybe where

instance Alt Maybe where
  Nothing <|> b = b
  a       <|> _ = a

instance Plus Maybe where
  zero = Nothing

instance MonadAlt Maybe where
  a <+> b = fold Just b a

instance MonadPlus Maybe where
  midentity = Nothing

instance Foldable Maybe where
  foldMap m = fold m identity

newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }
  -- MaybeT    :: m (Maybe a) -> MaybeT m a
  -- runMaybeT :: MaybeT m a  -> m (Maybe a)

instance MonadTrans MaybeT where
  liftM m = MaybeT (pure . pure =<< m)

instance MonadTransform MaybeT where
  transform f = MaybeT . f . runMaybeT

instance Eq a => Eq (Maybe a) where
  Just a == Just b   = a == b
  Nothing == Nothing = True
  _ == _             = False

instance Order a => Order (Maybe a) where
  compare (Just a) (Just b) = compare a b
  compare Nothing Nothing   = EQ
  compare Nothing _         = LT
  compare _ Nothing         = GT

instance MinBound (Maybe a) where
  minBound = Nothing

instance MaxBound a => MaxBound (Maybe a) where
  maxBound = Just maxBound

instance Bound a => Bound (Maybe a)
