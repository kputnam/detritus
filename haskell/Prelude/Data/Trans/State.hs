{-# LANGUAGE NoImplicitPrelude #-}

module Data.Trans.State where
import Data.Maybe
import Data.Id

class Monad m => MonadState s m | m -> s where
  -- | Yield the internal state
  get :: m s
  get = state (\s -> (s, s))

  -- | Replace the internal state
  put :: s -> m ()
  put s = state (\_ -> ((), s))

  -- | Embed a stateful action into the monad
  state :: (s -> (a, s)) -> m a
  state f = 

modify :: MonadState s m => (s -> s) -> m ()
modify f = undefined

gets :: MonadState s m => (s -> a) -> m a
gets = undefined

instance Monad m => MonadState s (L.StateT s m) where
  get   = L.get
  put   = L.put
  state = L.state

instance Monad m => MonadState s (S.StateT s m) where
  get   = S.get
  put   = S.put
  state = S.state

instance (Monad m, Monoid w) => MonadState s (LRWS.RWST r w s m) where
  get   = LRWS.get
  put   = LRWS.put
  state = LRWS.state

instance (Monad m, Monoid w) => MonadState s (SRWS.RWST r w s m) where
  get   = SRWS.get
  put   = SRWS.put
  state = SRWS.state

instance MonadState s m => MonadState s (ContT r m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance (Error e, MonadState s m) => MonadState s (ErrorT e m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance MonadState s m => MonadState s (IdT m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance MonadState s m => MonadState s (ListT m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance MonadState s m => MonadState s (MaybeT m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance MonadState s m => MonadState s (ReaderT e m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (L.WriterT w m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance (Monoid w, MonadState s m) => MonadState s (S.WriterT w m) where
  get   = lift get
  put   = lift . put
  state = lift . state
