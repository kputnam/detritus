{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Cofunctor where

class Cofunctor f where
  (-$) :: (a -> b) -> f b -> f a
