{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Compose where
import Data.Functor.Functor

newtype Compose f g a
  = Compose (f (g a))

loop = loop

instance (Functor f, Functor g) => Functor (Compose f g) where
  f +$ (Compose x) = Compose ((f +$) +$ x)
