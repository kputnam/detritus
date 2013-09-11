{-# LANGUAGE NoImplicitPrelude #-}

module Data.Semigroupoid.Semigroup where

infixr 6 <>

class Semigroup a where
  (<>) :: a -> a -> a
