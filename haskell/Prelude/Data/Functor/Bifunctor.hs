{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Bifunctor where

class Bifunctor f where
  ($$) :: (a -> a') -> (b -> b') -> f a b -> f a' b'
