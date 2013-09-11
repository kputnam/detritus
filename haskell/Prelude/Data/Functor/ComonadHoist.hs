{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.ComonadHoist where
import Data.Functor.Comonad
import Data.Id

class ComonadHoist t where
  cohoist :: Comonad f => t f a -> t Id a
