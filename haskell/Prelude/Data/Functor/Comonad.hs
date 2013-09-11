{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Comonad where
import Data.Functor.Extend
import Data.Semigroupoid.Monoid

class Extend f => Comonad f where
  -- Dual to Applicative 'pure'
  extract :: f a -> a

instance Monoid e => Comonad ((->) e) where
  extract f = f identity
