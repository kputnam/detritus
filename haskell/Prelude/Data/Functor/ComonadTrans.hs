{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.ComonadTrans where
import Data.Functor.Comonad

class ComonadTrans t where
  lowerM :: Comonad f => t f a -> f a
