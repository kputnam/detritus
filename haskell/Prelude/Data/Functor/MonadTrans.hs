{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.MonadTrans where
import Data.Function
import Data.Functor.Monad

class MonadTrans t where
  liftM :: Monad m => m a -> t m a

newtype ReaderT e m a
  = ReaderT { runReaderT :: e -> m a }

instance MonadTrans (ReaderT e) where
--liftM :: Monad m => m a -> ReaderT e m a
  liftM = ReaderT . const
