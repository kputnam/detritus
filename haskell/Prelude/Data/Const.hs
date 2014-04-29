{-# LANGUAGE NoImplicitPrelude #-}

module Data.Const where
import Data.Order.Bound
import Data.Functor.Functor
import Data.Functor.Cofunctor

newtype Const a b
  = Const { runConst :: a }
  -- Const    :: a -> Const a b
  -- runConst :: Const a b -> a

instance Functor (Const a) where
  _ +$ Const a = Const a

instance Cofunctor (Const a) where
  _ -$ Const a = Const a

instance MinBound a => MinBound (Const a b) where
  minBound = Const minBound

instance MaxBound a => MaxBound (Const a b) where
  maxBound = Const maxBound
