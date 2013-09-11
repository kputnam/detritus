{-# LANGUAGE NoImplicitPrelude #-}

module Data.Cont where
import Data.Function
import Data.Functor.Functor
import Data.Functor.Apply
import Data.Functor.Applicative
import Data.Functor.Bind
import Data.Functor.Monad
import Data.Functor.Extend

newtype Cont r a
  = Cont { runCont :: (a -> r) -> r }
  -- Cont    :: ((a -> r) -> r) -> Cont r a
  -- runCont :: Cont r a -> (a -> r) -> r

instance Functor (Cont r) where
  f +$ Cont ka = Cont (ka . (. f))

instance Apply (Cont r) where
  Cont kf <*> Cont ka = Cont (\kb -> kf (ka . (kb .)))

instance Bind (Cont r) where
  f =<< Cont ka = Cont (\kb -> ka (flip runCont kb . f))

instance Monad (Cont r) where

instance Applicative (Cont r) where
  pure a = Cont (\k -> k a)

instance Extend (Cont r) where
  extend f = pure . f
