{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}

module Trans
  ( Monoid(..), concat
  , Functor(..), (<$)
  , Applicative(..), (*>), (<*)
  , Monad(..), Lift(..)
  --, (<=<), (>=>), filterM, foldM, forM, forever, mapM
  --, replicateM, sequence, unless, when, zipWithM
  , Id(..)
  , Maybe(..), MaybeT(..), runMaybe
  , ListT(..), consT, foldT, toListT, unListT
  , ReaderT(..), ReaderOps(..), runReader
  , WriterT(..), execWriterT, evalWriterT
  , WriterOps(..), runWriter, execWriter, evalWriter
  , Either(..), EitherT(..), runEither
  , State(..), runState, evalState, execState
  , StateT(..), evalStateT, execStateT
  , StateOps(..)
  ) where

import qualified Control.Applicative as A
import qualified Prelude as P
import Prelude
  ( Num(..), Ord(..), Char(..), Show(..)
  , Eq(..), Enum(..), Bounded(..)
  , IO(..), getChar, getLine, putChar, putStr, putStrLn, print
  , id, flip, foldr, fst, snd )

infixl 4 <$>
infixl 4 <*>
infixr 1 =<<
infixr 6 <>

-- Monoid
--------------------------------------------------------------------------------

class Monoid m where
  (<>)  :: m -> m -> m
  empty :: m

concat :: Monoid m => [m] -> m
concat = foldr (<>) empty

-- Functor
--------------------------------------------------------------------------------

class Functor f where
  (<$>) :: (a -> b) -> f a -> f b

(<$) :: Functor f => a -> f b -> f a
(<$) a b = h <$> b
  where h b = a

-- Applicative
--------------------------------------------------------------------------------

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

(<*) :: Applicative f => f a -> f b -> f a
(<*) f g = h <$> f <*> g
  where h a b = a

(*>) :: Applicative f => f a -> f b -> f b
(*>) f g = h <$> f <*> g
  where h a b = b

-- Monad
--------------------------------------------------------------------------------

class Applicative f => Monad f where
  (=<<) :: (a -> f b) -> f a -> f b
  join  :: f (f a) -> f a

  -- Mutually recursive definitions
  f =<< x = join (f <$> x)
  join x  = id =<< x

-- (>>)       :: Monad m => m a -> m b -> m a
-- (<<)       :: Monad m => m a -> m b -> m b
-- (>>=)      :: Monad m => m a -> (a -> m b) -> m b
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

-- Lift
--------------------------------------------------------------------------------

class Lift t where
  lift :: Monad m => m a -> t m a

-- Unit
--------------------------------------------------------------------------------

instance Monoid () where
  empty  = ()
  _ <> _ = ()

-- IO
--------------------------------------------------------------------------------

instance Functor IO where
  (<$>) = (A.<$>)

instance Applicative IO where
  pure  = A.pure
  (<*>) = (A.<*>)

instance Monad IO where
  (=<<) = (P.=<<)

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

runMaybe :: b -> (a -> b) -> Maybe a -> b
runMaybe a _ Nothing  = a
runMaybe _ f (Just x) = f x

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
  f =<< x = runMaybe Nothing f x

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
  f =<< x = MaybeT (runMaybe nothing just =<< runMaybeT x)
    where nothing = pure Nothing
          just    = runMaybeT <$> f

instance Lift MaybeT where
  lift m = MaybeT (pure <$> m)

-- List
--------------------------------------------------------------------------------

instance Monoid [a] where
  empty  = []
  a <> b = foldr (:) b a

instance Functor [] where
  f <$> (x:xs) = f x : (f <$> xs)
  f <$> _      = empty

instance Applicative [] where
  pure a = [a]
  (f:fs) <*> (x:xs) = f x : (fs <*> xs)
  _      <*> _      = empty

instance Monad [] where
  f =<< xs = foldr (\x bs -> f x <> bs) empty xs

-- ListT
--------------------------------------------------------------------------------

-- newtype ListT m a
--   = ListT { runListT :: m [a] }

newtype ListT m a
  = ListT { runListT :: m (Maybe (a, ListT m a)) }

toListT :: Monad m => [m a] -> ListT m a
toListT xs = foldr consT empty xs

unListT :: Monad m => ListT m a -> m [a]
unListT xs = foldT f (pure empty) xs
  where f a as = (a:) <$> as

foldT :: Monad m => (a -> m b -> m b) -> m b -> ListT m a -> m b
foldT f z xs = g =<< runListT xs
  where g m       = runMaybe z c m
        c (a, as) = f a (foldT f z as)

consT :: Monad m => m a -> ListT m a -> ListT m a
consT m ms = ListT (g =<< m)
  where g a = pure (Just (a, ms))

instance Monad m => Monoid (ListT m a) where
  empty  = ListT (pure Nothing)
  a <> b = ListT (runMaybe z f =<< runListT a)
    where f (a, as) = pure (Just (a, as <> b))
          z         = runListT b

instance Functor m => Functor (ListT m) where
  f <$> xs = ListT ((f *** rec <$>) <$> runListT xs)
    where rec xs  = f <$> xs
          f *** g = \(a, b) -> (f a, g b)

instance Applicative m => Applicative (ListT m) where
  pure x    = ListT (pure (pure (x, ListT (pure Nothing))))
  fs <*> xs = ListT (oq <$> runListT fs <*> runListT xs)
    where oq x y = op <$> x <*> y
          op (f, fs) (x, xs) = (f x, fs <*> xs)

instance Monad m => Monad (ListT m) where
  f =<< xs = P.error "todo"

instance Lift ListT where
  lift m = ListT (f <$> m)
    where f a = Just (a, ListT (pure Nothing))

-- Reader
--------------------------------------------------------------------------------

runReader :: (e -> a) -> e -> a
runReader x = x

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
  f =<< x = ReaderT (\e -> g e =<< runReaderT x e)
    where g e = flip runReaderT e <$> f

instance Lift (ReaderT e) where
  lift m = ReaderT (pure m)

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

runWriter :: (w, a) -> (w, a)
runWriter x = x

execWriter :: (w, a) -> w
execWriter (w, _) = w

evalWriter :: (w, a) -> a
evalWriter (_, a) = a

instance Functor ((,) w) where
  f <$> x = (w, f a)
    where (w, a) = x

instance Monoid w => Applicative ((,) w) where
  pure x  = (empty, x)
  f <*> x = (wf <> wa, g a)
    where (wf, g) = f
          (wa, a) = x

instance Monoid w => Monad ((,) w) where
  f =<< x = (wb <> wx, b)
    where (wx, a) = x
          (wb, b) = f a

-- WriterT
--------------------------------------------------------------------------------

execWriterT :: Functor m => WriterT w m a -> m w
execWriterT x = fst <$> runWriterT x

evalWriterT :: Functor m => WriterT w m a -> m a
evalWriterT x = snd <$> runWriterT x

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

instance Monoid w => Lift (WriterT w) where
  lift m = WriterT (pure <$> m)

-- WriterOps
--------------------------------------------------------------------------------

class (Monoid w, Monad m) => WriterOps w m | m -> w where
  tell   :: w -> m ()
  writer :: (w, a) -> m a

instance Monoid w => WriterOps w ((,) w) where
  tell w   = (w, ())
  writer x = x

instance (Monoid w, Monad m) => WriterOps w (WriterT w m) where
  tell w   = WriterT (pure (w, ()))
  writer x = WriterT (pure x)

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

runEither :: (a -> c) -> (b -> c) -> Either a b -> c
runEither f _ (Left a)  = f a
runEither _ f (Right b) = f b

-- EitherT
--------------------------------------------------------------------------------

newtype EitherT e m a
  = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  f <$> x = EitherT ((f <$>) <$> runEitherT x)

instance Applicative m => Applicative (EitherT a m) where
  pure x  = EitherT (pure (pure x))
  f <*> x = EitherT ((<*>) <$> runEitherT f <*> runEitherT x)

instance Monad m => Monad (EitherT a m) where
  f =<< x = P.error "todo"

instance Lift (EitherT a) where
  lift m = EitherT (pure <$> m)

-- State
--------------------------------------------------------------------------------

-- type State s a
--   = StateT s Id a

newtype State s a
  = State { runState :: s -> (s, a) }

instance Functor (State s) where
  f <$> x = State (\s0 -> f <$> runState x s0)

instance Applicative (State s) where
  pure x  = State (\s0 -> (s0, x))
  f <*> x = State (\s0 -> let (s1, f') = runState f s0
                              (s2, x') = runState x s1
                           in (s2, f' x'))

instance Monad (State s) where
  f =<< x = State (\s0 -> let (s1, x') = runState x s0
                           in runState (f x') s1)

execState :: State s a -> s -> s
execState x s = fst (runState x s)

evalState :: State s a -> s -> a
evalState x s = snd (runState x s)

-- StateT
--------------------------------------------------------------------------------

execStateT :: Functor m => StateT s m a -> s -> m s
execStateT x s = fst <$> runStateT x s

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT x s = snd <$> runStateT x s

-- Hmm! There's no nice instances to reuse!
newtype StateT s m a
  = StateT { runStateT :: s -> m (s, a) }

instance Functor m => Functor (StateT s m) where
  f <$> x = StateT (\s -> (f <$>) <$> runStateT x s)
  --      = StateT (((f <$>) <$>) <$> runStateT x)

instance Applicative m => Applicative (StateT s m) where
  pure x  = StateT (\s -> pure (s, x))
  f <*> x = StateT (\s -> op <$> runStateT f s)
    where op = P.error "todo"
    -- f :: s -> m (s, a -> b)
    -- x :: s -> m (s, a)

instance Monad m => Monad (StateT s m) where
  f =<< x = StateT (\s -> g =<< runStateT x s)
  --      = StateT ((=<<) <$> pure g <*> runStateT x)
    where g (s, a) = runStateT (f a) s

instance Lift (StateT s) where
  lift m = StateT (\s -> (\a -> (s, a)) <$> m)

-- StateOps
--------------------------------------------------------------------------------

class StateOps s m | m -> s where
  get   :: m s
  put   :: s -> m ()
  state :: (s -> (s, a)) -> m a

instance StateOps s (State s) where
  get     = State (\s -> (s, s))
  put s   = State (\_ -> (s, ()))
  state f = State (\s -> f s)

instance Monad m => StateOps s (StateT s m) where
  get     = StateT (\s -> pure (s, s))
  put s   = StateT (\_ -> pure (s, ()))
  state f = StateT (\s -> pure (f s))
