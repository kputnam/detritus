{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Apply where
import Data.Functor.Functor

infixl 4 <*>

class Functor f => Apply f where
  (<*>) :: f (a -> b) -> f a -> f b

instance Apply ((->) e) where
  (<*>) f g x = f x (g x)
