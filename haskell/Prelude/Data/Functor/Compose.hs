{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Compose where

newtype Compose f g a
  = Compose (f (g a))
