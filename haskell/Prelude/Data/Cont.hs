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

reset :: Cont r r -> r
reset (Cont k) = k id

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = Cont (\k -> reset (f k))

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

newtype ContT r m a
  = ContT { runContT :: (a -> m r) -> m r }
  -- ContT    :: ((a -> m r) -> m r) -> Cont r m a
  -- runContT :: ContT r m a -> (a -> m r) -> m r

resetT :: Applicative m => ContT r m r -> m r
resetT (ContT k) = k pure

shiftT :: Applicative m => ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT f = ContT (\k -> resetT (f k))

instance Functor (ContT r m) where
  f +$ ContT ka = ContT (ka . (. f))

instance Apply (ContT r m) where
  ContT kf <*> ContT ka = ContT (\kb -> kf (ka . (kb .)))

instance Bind (ContT r m) where
  f =<< ContT ka = ContT (\kb -> ka (flip runContT kb . f))

instance Applicative (ContT r m) where
  pure a = ContT (\k -> k a)

instance Extend (ContT m r) where
  extend f = pure . f
