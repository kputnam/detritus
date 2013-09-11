{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes           #-}

module Compose
  ( Compose(..)
  , (:.)
  , liftI
  , liftO
  , hoistI
  , hoistO
  ) where

import Control.Applicative

newtype Compose f g a
  = Compose (f (g a))

infix :.
type f :. g
  = Compose f g

instance (Functor f, Functor g) => Functor (f :. g) where
  fmap = undefined

instance (Applicative f, Applicative g) => Applicative (f :. g) where
  pure                    = Compose . pure . pure
  Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)


hole :: a
hole = undefined

-- | Lift an action from the outer functor into the composite.
-- Alternative reading: append an 'Applicative' to the right of @f@.
liftO :: (Functor f, Applicative g) => f a -> (f :. g) a
liftO = hole

-- | Lift an action from the inner functor into the composite.
-- Alternative reading: prepend an 'Applicative' to the left of @g@.
liftI :: Applicative f => g a -> (f :. g) a
liftI = hole

-- | Lift a natural transformation from @g@ to @h@ into a morphism
-- from @f :. g@ to @h :. g@.
hoistO :: (forall x. f x -> h x) -> (f :. g) a -> (h :. g) a
hoistO = hole

-- | Lift a natural transformation from @g@ to @h@ into a morphism
-- from @f :. g@ to @f :. h@.
hoistI :: (forall x. g x -> h x) -> (f :. g) a -> (f :. h) a
hoistI = hole
