{-# LANGUAGE NoImplicitPrelude #-}

module Data.List where
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
import Data.Functor.Foldable
import Data.Functor.MonadTrans
import Data.Functor.MonadTransform
import Data.Bool
import Data.Order.Eq
import Data.Order.Order
import Data.Order.Bound

data List a
  = Cons a (List a)
  | Nil

fold :: (a -> r -> r) -> r -> List a -> r
fold f z (Cons a as) = a `f` fold f z as
fold _ z Nil         = z

instance Semigroup (List a) where
  as <> bs = fold Cons bs as

instance Monoid (List a) where
  identity = Nil

instance Functor List where
  f +$ as = fold (Cons . f) Nil as

instance Apply List where
  fs <*> as = fold ((<>) . (+$ as)) Nil fs

instance Applicative List where
  pure a = Cons a Nil

instance Bind List where
  f =<< as = fold ((<>) . f) Nil as

instance Monad List where

instance Alt List where
  Nil <|> bs = bs
  as  <|> _  = as

instance Plus List where
  zero = Nil

instance MonadAlt List where
  as <+> bs = fold Cons bs as

instance MonadPlus List where
  midentity = Nil

instance Foldable List where
  foldMap m = fold ((<>) . m) identity

newtype ListT m a
  = ListT { runListT :: m (List a) }

instance MonadTrans ListT where
  liftM m = ListT (pure . pure =<< m)

instance MonadTransform ListT where
--transform :: (Monad f, Monad g) => (forall z. f z -> g z) -> ListT f a -> ListT g a
  transform _ _ = let x = x in x

instance Eq a => Eq (List a) where
  Nil == Nil             = True
  Cons a as == Cons b bs = a == b && as == bs
  _ == _                 = False

instance Order a => Order (List a) where
  compare Nil _ = LT
  compare _ Nil = GT
  compare (Cons a as) (Cons b bs) = case compare a b of
    LT -> LT
    GT -> GT
    EQ -> compare as bs

instance MinBound a => MinBound (List a) where
  minBound = Nil

instance MaxBound a => MaxBound (List a) where
  maxBound = Cons maxBound maxBound

instance (MinBound a, MaxBound a) => Bound (List a)
