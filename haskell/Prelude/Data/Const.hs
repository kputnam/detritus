{-# LANGUAGE NoImplicitPrelude #-}

module Data.Const where
import Data.Functor.Functor
import Data.Functor.Contravariant

newtype Const a b
  = Const { runConst :: a }
  -- Const    :: a -> Const a b
  -- runConst :: Const a b -> a

instance Functor (Const a) where
  _ +$ Const a = Const a

instance Contravariant (Const a) where
  _ -$ Const a = Const a
