{-# LANGUAGE NoImplicitPrelude #-}

module Data.Function
  ( id
  , (.)
  , ($)
  , on
  , fix
  , flip
  , const
  ) where
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Category

infixr 0 $

($) :: (a -> b) -> a -> b
f $ a = f a

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on (+) f x y = f x + f y

fix :: (a -> a) -> a
fix f = f (fix f)

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

const :: a -> e -> a
const a _ = a
