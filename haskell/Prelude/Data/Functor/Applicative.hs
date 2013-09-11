{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Applicative where
import Data.Functor.Apply

class Apply f => Applicative f where
  -- Dual to Comonad 'extract'
  pure :: a -> f a

instance Applicative ((->) e) where
  pure a _ = a
