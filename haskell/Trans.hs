{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Trans where

import Prelude (Num(..), Char(..), Show(..), Eq(..), id, flip, foldr, error)
import qualified Prelude as P

-- Monoid
--------------------------------------------------------------------------------

class Monoid m where
  (<>)  :: m -> m -> m
  empty :: m

-- concat :: Monoid m => [m] -> m

-- Functor
--------------------------------------------------------------------------------

class Functor f where
  (<$>) :: (a -> b) -> f a -> f b
-- (<$) :: Functor f => a -> f b -> f a

-- Applicative
--------------------------------------------------------------------------------

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-- (<*) :: f a -> f b -> f a
-- (*>) :: f a -> f b -> f b

-- Monad
--------------------------------------------------------------------------------

class Applicative f => Monad f where
  (=<<) :: (a -> f b) -> f a -> f b
  join  :: f (f a) -> f a
  
  -- Mutually recursive definitions
  f =<< x = join (f <$> x)
  join x  = id =<< x

-- (<=<)      :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
-- (>=>)      :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- filterM    :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- foldM      :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
-- forM       :: Monad m => [a] -> (a -> m b) -> m [b]
-- forever    :: Monad m => m a -> m b
-- mapM       :: Monad m => (a -> m b) -> [a] -> m [b]
-- replicateM :: Monad m => Int -> m a -> m [a]
-- sequence   :: Monad m => [m a] -> m [a]
-- unless     :: Monad m => Bool -> m () -> m ()
-- when       :: Monad m => Bool -> m () -> m ()
-- zipWithM   :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]

-- Unit
--------------------------------------------------------------------------------

instance Monoid () where
  empty   = ()
  _ <> _  = ()

-- Id
--------------------------------------------------------------------------------

newtype Id a
  = Id { runId :: a }
  deriving (Show, Eq)

instance Monoid a => Monoid (Id a) where
  empty = Id empty
  Id a <> Id b = Id (a <> b)

instance Functor Id where
  f <$> Id x = Id (f x)

instance Applicative Id where
  pure x = Id x
  Id f <*> Id x = Id (f x)

instance Monad Id where
  f =<< Id x = f x

-- Maybe
--------------------------------------------------------------------------------

data Maybe a
  = Nothing
  | Just a
  deriving (Show, Eq)

instance Monoid a => Monoid (Maybe a) where
  empty = Nothing
  Nothing <> b = b
  a <> Nothing = a
  Just a <> Just b = Just (a <> b)

instance Functor Maybe where
  f <$> Just x = Just (f x)
  f <$> _      = Nothing

instance Applicative Maybe where
  pure a = Just a
  Just f <*> Just x = Just (f x)
  _      <*> _      = Nothing

instance Monad Maybe where
  f =<< x = maybe Nothing f x

-- MaybeT
--------------------------------------------------------------------------------

newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  f <$> x = MaybeT ((f <$>) <$> runMaybeT x)

instance Applicative m => Applicative (MaybeT m) where
  pure x  = MaybeT (pure (pure x))
  f <*> x = MaybeT ((<*>) <$> runMaybeT f <*> runMaybeT x)

instance Monad m => Monad (MaybeT m) where
  f =<< x = MaybeT (maybe nothing just =<< runMaybeT x)
    where nothing = pure Nothing    --
          just a  = runMaybeT (f a) --
    -- f =<< x = maybe Nothing f x

-- MaybeOps
--------------------------------------------------------------------------------

maybe :: b -> (a -> b) -> Maybe a -> b
maybe a _ Nothing  = a
maybe _ f (Just x) = f x

orElse :: Maybe a -> a -> a
orElse m a = maybe a id m

-- List
--------------------------------------------------------------------------------

instance Monoid [a] where
  empty = []
  [] <> bs      = bs
  (a:as) <> bs  = a : (as <> bs)

instance Functor [] where
  f <$> (x:xs) = f x : (f <$> xs)
  f <$> _      = []

instance Applicative [] where
  pure a = [a]
  (f:fs) <*> (x:xs) = f x : (fs <*> xs)
  _      <*> _      = []

instance Monad [] where
  f =<< xs = foldr (\x bs -> f x <> bs) [] xs

-- ListT
--------------------------------------------------------------------------------

newtype ListT m a
  = ListT { runListT :: m [a] }

instance Functor m => Functor (ListT m) where
  f <$> x = ListT ((f <$>) <$> runListT x)

instance Applicative m => Applicative (ListT m) where
  pure x  = ListT (pure (pure x))
  f <*> x = ListT ((<*>) <$> runListT f <*> runListT x)

instance Monad m => Monad (ListT m) where
  f =<< x = ListT (g =<< runListT x)
    where g as   = foldr h (pure []) as
          h a bs = (<>) <$> runListT (f a) <*> bs
    -- f =<< xs = foldr (\x bs -> f x <> bs) [] xs

-- ListOps
--------------------------------------------------------------------------------



-- Reader
--------------------------------------------------------------------------------

instance Monoid a => Monoid (e -> a) where
  empty e    = empty
  (<>) f g e = f e <> g e

instance Functor ((->) e) where
  (<$>) f g e = f (g e)   -- function composition!

instance Applicative ((->) e) where
  pure x e    = x         -- the K combinator!
  (<*>) f g e = f e (g e) -- the S combinator!

instance Monad ((->) e) where
  (=<<) f g e = f (g e) e

-- ReaderT
--------------------------------------------------------------------------------

newtype ReaderT e m a
  = ReaderT { runReaderT :: e -> m a }

instance Functor m => Functor (ReaderT e m) where
  f <$> x = ReaderT (\e -> f <$> runReaderT x e)

instance Applicative m => Applicative (ReaderT e m) where
  pure x  = ReaderT (pure (pure x))
  f <*> x = ReaderT ((<*>) <$> runReaderT f <*> runReaderT x)

instance Monad m => Monad (ReaderT e m) where
  f =<< x = ReaderT ((=<<) <$> g <*> runReaderT x)
    where g e a = runReaderT (f a) e
    -- (=<<) f g e = f (g e) e

-- ReaderOps
--------------------------------------------------------------------------------

class ReaderOps e m | m -> e where
  ask    :: m e
  local  :: (e -> e) -> m a -> m a
  reader :: (e -> a) -> m a

instance ReaderOps e ((->) e) where
  ask e       = e
  local f x e = x (f e)
  reader f e  = f e

instance Monad m => ReaderOps e (ReaderT e m) where
  ask       = ReaderT (\e -> pure e)
  local f g = ReaderT (\e -> runReaderT g (f e))
  reader f  = ReaderT (\e -> pure (f e))

-- Writer
--------------------------------------------------------------------------------

instance Functor ((,) w) where
  f <$> x = (wf, f xx)
    where
      (wf, xx) = x

instance Monoid w => Applicative ((,) w) where
  pure x  = (empty, x)
  f <*> x = (wx <> wf, xf xx)
    where (wf, xf) = f
          (wx, xx) = x

instance Monoid w => Monad ((,) w) where
  f =<< x = (wx <> wf, xf)
    where (wx, xx) = x
          (wf, xf) = f xx

-- WriterT
--------------------------------------------------------------------------------

newtype WriterT w m a
  = WriterT { runWriterT :: m (w, a) }

instance Functor m => Functor (WriterT w m) where
  f <$> x = WriterT ((f <$>) <$> runWriterT x)

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure x  = WriterT (pure (pure x))
  f <*> x = WriterT ((<*>) <$> runWriterT f <*> runWriterT x)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  f =<< x = WriterT (g =<< runWriterT x)
    where g (w, a)   = h w <$> runWriterT (f a)
          h w (v, a) = (w <> v, a)
    -- f =<< x = (wx <> wf, xf)
    --   where (wx, xx) = x
    --         (wf, xf) = f xx

-- WriterOps
--------------------------------------------------------------------------------

class (Monoid w, Monad m) => WriterOps w m | m -> w where
  tell   :: w -> m ()
  writer :: (w, a) -> m a

instance Monoid w => WriterOps w ((,) w) where
  tell w   = (w, ())
  writer x = x

-- Either
--------------------------------------------------------------------------------

data Either a b
  = Left  a
  | Right b
  deriving (Show, Eq)

instance Functor (Either a) where
  f <$> Right x = Right (f x)
  f <$> Left x  = Left x

instance Applicative (Either a) where
  pure x = Right x
  Right f <*> Right x = Right (f x)
  _       <*> Left x  = Left x
  Left f  <*> _       = Left f

instance Monad (Either a) where
  f =<< Right x = f x
  f =<< Left x  = Left x

-- EitherT
--------------------------------------------------------------------------------

newtype EitherT a m b
  = EitherT { runEitherT :: Either a (m b) }

instance Functor m => Functor (EitherT a m) where
  f <$> x = EitherT ((f <$>) <$> runEitherT x)

instance Applicative m => Applicative (EitherT a m) where
  pure x  = EitherT (pure (pure x))
  f <*> g = EitherT ((<*>) <$> runEitherT f <*> runEitherT g)

-- State
--------------------------------------------------------------------------------

newtype State s a
  = State { runState :: s -> (s, a) }

instance Functor (State s) where
  f <$> x = State (\s -> f <$> runState x s)

instance Applicative (State s) where
  pure x  = State (\s -> (s, x))
  f <*> x = State (\s -> let (sx, ax) = runState x s
                             (sf, af) = runState f sx
                          in (sf, af ax))

instance Monad (State s) where
  f =<< x = State (\s -> let (sx, ax) = runState x s
                          in runState (f ax) sx)

-- StateT
--------------------------------------------------------------------------------

-- Hmm! There's no nice instances to reuse!
newtype StateT s m a
  = StateT { runStateT :: s -> m (s, a) }

instance Functor m => Functor (StateT s m) where
  f <$> x = StateT (\s -> (f <$>) <$> runStateT x s)
         -- StateT (((f <$>) <$>) <$> runStateT x)

instance Applicative m => Applicative (StateT s m) where
  pure x  = StateT (\s -> pure (s, x))
  -- TODO (<*>)

-- StateOps
--------------------------------------------------------------------------------

class StateOps s m | m -> s where
  get   :: m s
  put   :: s -> m ()
  state :: (s -> (s, a)) -> m a

instance StateOps s (State s) where
  get     = State (\s -> (s, s))
  put s   = State (\_ -> (s, ()))
  state f = State f

instance Monad m => StateOps s (StateT s m) where
  get     = StateT (\s -> pure (s, s))
  put s   = StateT (\_ -> pure (s, ()))
  state f = StateT (\s -> pure (f s))
