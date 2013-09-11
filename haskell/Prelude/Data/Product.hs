{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Product where
import Data.Functor.Functor
import Data.Functor.Apply
import Data.Functor.Applicative
import Data.Functor.Bind
import Data.Functor.Monad
import Data.Functor.MonadTrans
import Data.Functor.MonadTransform
import Data.Functor.Comonad
import Data.Functor.Extend
import Data.Functor.Bifunctor
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Semigroup
import Data.Semigroupoid.Monoid
import Data.Bool
import Data.Order.Eq
import Data.Order.Order
import Data.Order.Bound

infixl 7 :*

data a :* b
  = (:*) a b

fst :: (a :* b) -> a
fst (a :* _) = a

snd :: (a :* b) -> b
snd (_ :* b) = b

-- | tell w is an action that produces the output w
tell :: Monoid w => w -> (w :* ())
tell w = w :* ()

-- | listen m is an action that executes the action m and adds its output to the
--   value of the computation
listen :: Monoid w => (w :* a) -> (w :* (a :* w))
listen (w :* a) = w :* (a :* w)

-- | listens f m is an action that executes the action m and adds the result of
--   applying f to the output to the value of the computation
listens :: Monoid w => (w -> b) -> (w :* a) -> (w :* (a :* b))
listens f (w :* a) = w :* (a :* f w)

-- | pass m is an action that executes the action m, which returns a value and a
--   function, and returns the value, applying the function to the output
pass :: (w :* (a :* (w -> w))) -> (w :* a)
pass (w :* (a :* f)) = f w :* a

-- | censor f m is an action that executes the action m and applies the function
--   f to its output, leaving the return value unchanged
censor :: Monoid w => (w -> w) -> (w :* a) -> (w :* a)
censor f (w :* a) = f w :* a

-- Writer
instance Functor ((:*) w) where
  (+$) f (w :* a) = w :* f a

instance Monoid w => Apply ((:*) w) where
  (w :* f) <*> (w' :* a) = (w <> w') :* f a

instance Monoid w => Applicative ((:*) w) where
  pure = (:*) identity

instance Monoid w => Bind ((:*) w) where
  f =<< (w :* a) = let (w' :* b) = f a
                    in (w <> w') :* b

instance Monoid w => Monad ((:*) w) where

newtype ProductT c m a
  = ProductT { runProductT :: m (c :* a) }

instance Monoid c => MonadTrans (ProductT c) where
  liftM m = ProductT (pure . pure =<< m)

instance MonadTransform (ProductT c) where
  transform f = ProductT . f . runProductT

instance Comonad ((:*) c) where
  extract (_ :* a) = a

instance Extend ((:*) c) where
  extend f (c :* a) = c :* f (c :* a)

instance (Semigroup a, Semigroup b) => Semigroup (a :* b) where
  (a :* b) <> (a' :* b') = (a <> a') :* (b <> b')

instance (Monoid a, Monoid b) => Monoid (a :* b) where
  identity = identity :* identity

-- Equivalent to Tensor (->)
instance Bifunctor (:*) where
  ($$) f g (a :* b) = f a :* g b

instance (Eq a, Eq b) => Eq (a :* b) where
  (a :* b) == (a' :* b') = (a == a') && (b == b')

instance (Order a, Order b) => Order (a :* b) where
  compare (a :* b) (a' :* b') = case compare a a' of
    EQ -> compare b b'
    x  -> x

instance (MinBound a, MinBound b) => MinBound (a :* b) where
  minBound = minBound :* minBound

instance (MaxBound a, MaxBound b) => MaxBound (a :* b) where
  maxBound = maxBound :* maxBound

instance (Bound a, Bound b) => Bound (a :* b) where
