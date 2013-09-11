{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.ExtendHoist where
import Data.Functor.ComonadHoist
import Data.Functor.Extend
import Data.Id

class ComonadHoist t => ExtendHoist t where
  cohoistS :: Extend f => t f a -> t Id a
