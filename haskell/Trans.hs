{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}

module Trans
  ( Monoid(..), concat
  , Functor(..), (<$)
  , Bifunctor(..), first, second
  , Applicative(..), (*>), (<*)
  , Biplicative(..), (**>), (<**)
  , Monad(..), Lift(..)
  --, (<=<), (>=>), filterM, foldM, forM, forever, mapM
  --, replicateM, sequence, unless, when, zipWithM
  , Id(..)
  , Maybe(..), runMaybe
  , MaybeT(..)
  , ListT(..), consT, foldT, toListT, unListT
  , runReader
  , ReaderT(..)
  , ReaderOps(..)
  , runWriter, execWriter, evalWriter
  , WriterT(..), execWriterT, evalWriterT
  , WriterOps(..)
  , Either(..), runEither
  , EitherT(..)
  , EitherOps(..)
  , State(..), runState, evalState, execState
  , StateT(..), evalStateT, execStateT
  , StateOps(..)
  ) where

import qualified Control.Applicative as A
import qualified Prelude as P
import Prelude
  ( Num(..), Ord(..), Char(..)
  , Eq(..), Enum(..), Bounded(..)
  , Show(..), showString, showParen
  , IO(..), getChar, getLine, putChar, putStr, putStrLn, print
  , id, flip, (.), ($), foldr, fst, snd )

infixl 4 <$>
infixl 4 <$
infixl 4 <*>
infixl 4 *>
infixl 4 <*
infixr 1 =<<
-- infixl 1 >>=
-- infixr 1 >=>
-- infixr 1 <=<
infixr 6 <>

-- Monoid
--------------------------------------------------------------------------------
-- Monoids are sets extended with an associative binary operation @<>@ and an
-- identity element @empty@ such that:
--
-- @<>@ is associative
--    forall a:m, b:m, c:m. a <> (b <> c) == (a <> b) <> c
--
-- @empty@ is the left and right identity of @<>@
--    forall a:m. empty <> a == a
--    forall a:m. a <> empty == a

class Monoid m where
  -- | An associative binary operation combining two values
  (<>)  :: m -> m -> m

  -- | The left and right identity of @<>@
  empty :: m

concat :: Monoid m => [m] -> m
concat = foldr (<>) empty

-- Functor
--------------------------------------------------------------------------------
-- Functors map "pure" functions @a -> b@ to "effectful" functions @f a -> f b@
-- such that:
--
--    (id <$>)    == id
--    (g . f <$>) == (g <$>) . (f <$>)

class Functor f where
  -- | Lifts the given pure function into @f@.
  (<$>) :: (a -> b) -> f a -> f b

-- | Replaces all @b@ locations with the same given @a@ value.
(<$) :: Functor f => a -> f b -> f a
(<$) a b = h <$> b
  where h b = a

-- Bifunctor
--------------------------------------------------------------------------------
class Bifunctor f where
  (<$$>) :: (a -> c) -> (b -> d) -> f a b -> f c d

first :: Bifunctor f => (a -> c) -> f a b -> f c b
first f x = (f <$$> id) x

second :: Bifunctor f => (b -> d) -> f a b -> f a d
second g x = (id <$$> g) x

-- Applicative
--------------------------------------------------------------------------------
-- An applicative functor is a functor with application providing operations
-- to lift "pure" values into @f@ and sequence computations in @f@ and their
-- results, such that the following laws are satisfied:
--
-- identity
--   pure id <*> x = x
--
-- composition
--   pure (.) <*> x <*> y <*> z = u <*> (v <*> w)
--
-- homomorphism
--   pure f <*> pure x = pure (f x)
--
-- interchange
--   f <*> pure x = pure ($ x) <*> f

class Functor f => Applicative f where
  -- | Lift a pure value into @f@.
  pure  :: a -> f a

  -- | Given an effectful computation that produces a function @a -> b@, apply
  -- it to the @a@ value produced by the other effectful computation.
  (<*>) :: f (a -> b) -> f a -> f b

-- | Sequence two computations, discarding the value produced by the second
(<*) :: Applicative f => f a -> f b -> f a
f <* g = h <$> f <*> g
  where h a b = a

-- | Sequence two computations, discarding the value produced by the first
(*>) :: Applicative f => f a -> f b -> f b
f *> g = h <$> f <*> g
  where h a b = b

-- Biplicative
--------------------------------------------------------------------------------
class Bifunctor f => Biplicative f where
  (<**>) :: f (a -> c) (b -> d) -> f a b -> f c d

-- | Sequence two computations, discarding the value produced by the second
(<**) :: Biplicative f => f a b -> f c d -> f a b
f <** g = (h <$$> h) f <**> g
  where h a b = a

-- | Sequence two computations, discarding the value produced by the first
(**>) :: Biplicative f => f a b -> f c d -> f c d
f **> g = (h <$$> h) f <**> g
  where h a b = b

-- Monad
--------------------------------------------------------------------------------
-- Monads are applicative functors whose computational structure can depend on
-- the results of previous computations.

class Applicative f => Monad f where
  (=<<) :: (a -> f b) -> f a -> f b
  join  :: f (f a) -> f a

  -- Mutually recursive definitions
  f =<< x = join (f <$> x)
  join x  = id =<< x

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
-- Represents a computation that produces a value of type @a@. This is a trivial
-- wrapper around @a@, but can be used with classes or functions parameterized
-- over a functor, applicative, or monad.

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
-- Represents a computation that either produces a value of type @a@ or instead
-- produces nothing. This can be useful for modeling partial functions.

data Maybe a
  = Nothing
  | Just a
  deriving (Show, Eq)

-- | Unwrap a @Maybe a@ value by providing a default value of type @b@ and a
-- function to map an @a@ to a @b@.
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
-- Describes a computation with @m@ effects that produces computation with
-- @Maybe@ effects. For example, @MaybeT ((->) Char) Int@ is a computation
-- that has access to a @Char@ environment value and produces a @Maybe Int@
-- computation.
--
-- Or another example, @MaybeT ((,) [Int]) Bool@ is a computation that emits a
-- list of Int values as an effect of producing a @Maybe Bool@ computation.

newtype MaybeT m a
  = MaybeT { runMaybeT :: m (Maybe a) }

instance Show (m (Maybe a)) => Show (MaybeT m a) where
  showsPrec p x = showParen (p > 10) (showString "MaybeT " . showsPrec 11 (runMaybeT x))

instance Functor m => Functor (MaybeT m) where
  f <$> x = MaybeT ((f <$>) <$> runMaybeT x)

instance Applicative m => Applicative (MaybeT m) where
  pure x  = MaybeT (pure (pure x))
  f <*> x = MaybeT ((<*>) <$> runMaybeT f <*> runMaybeT x)

instance Monad m => Monad (MaybeT m) where
  f =<< x = MaybeT (runMaybe n j =<< runMaybeT x)
    where n = pure Nothing
          j = runMaybeT <$> f

instance Lift MaybeT where
  -- Convert a computation with @m@ effects that produces an @a@ value into one
  -- which has @m@ effects and produces a @Maybe a@ computation. The name "lift"
  -- is confusing because the given argument is not a partial function that is
  -- lifted into @m@. That would be done by @m@'s monad transformer's @lift@!
  --
  -- Instead, this embeds the @m@ computation into the @MaybeT m@ monad, without
  -- using any special features of @MaybeT m@ (there are none). Or, said another
  -- way, @lift@ embeds @Maybe@ effects into @m@, without using any interesting
  -- features of @Maybe@.
  lift m = MaybeT (Just <$> m)

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
-- Describes a computation with @m@ effects that produces computation with the
-- effects of @[]@. For example, @ListT (Either Char) Int@ is a computation
-- that has access to a @Char@ environment value and produces a @Maybe Int@.

-- newtype ListT m a
--   = ListT { runListT :: m [a] }

newtype ListT m a
  = ListT { runListT :: m (Maybe (a, ListT m a)) }

instance Show (m (Maybe (a, ListT m a))) => Show (ListT m a) where
  showsPrec p x = showParen (p > 10) (showString "ListT " . showsPrec 11 (runListT x))

toListT :: Monad m => [m a] -> ListT m a
toListT xs = foldr consT empty xs

-- Sequences the effects in each cons cell into one effect. Converting
-- @ListT m a@ to @[m a]@ requires a separate function for each @m@.
unListT :: Monad m => ListT m a -> m [a]
unListT xs = foldT f (pure empty) xs
  where f a as = (a:) <$> as

-- Sequences the effects in each cons cell into one effect.
foldT :: Monad m => (a -> m b -> m b) -> m b -> ListT m a -> m b
foldT f z xs = g =<< runListT xs
  where g m       = runMaybe z c m
        c (a, as) = f a (foldT f z as)

-- Prepend an element at the beginning of the list
consT :: Monad m => m a -> ListT m a -> ListT m a
consT m ms = ListT (g =<< m)
  where g a = pure (Just (a, ms))

instance Monad m => Monoid (ListT m a) where
  empty  = ListT (pure Nothing)
  a <> b = ListT (runMaybe z f =<< runListT a)
    where f (h, t) = pure (Just (h, t <> b))
       -- f        = pure <$> Just <$> second (<> b)
          z        = runListT b

instance Functor m => Functor (ListT m) where
  f <$> xs = ListT ((f <$$> rec <$>) <$> runListT xs)
    where rec xs  = f <$> xs

instance Applicative m => Applicative (ListT m) where
  pure x    = ListT (pure (Just (x, ListT (pure Nothing))))
  fs <*> xs = ListT (oq <$> runListT fs <*> runListT xs)
    where oq x y = op <$> x <*> y
          op (f, fs) (x, xs) = (f x, fs <*> xs)

instance Monad m => Monad (ListT m) where
  f =<< xs = ListT (runMaybe n j =<< runListT xs)
    where n        = pure Nothing
          j (h, t) = runListT (f h <> (f =<< t))

instance Lift ListT where
  lift m = ListT (f <$> m)
    where f a = Just (a, ListT (pure Nothing))

-- Reader
--------------------------------------------------------------------------------
-- Represents a computation which can read values from a shared environment
-- and execute subcomputations under a modified environment

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

class Monad m => ReaderOps e m | m -> e where
  ask    :: m e
  local  :: (e -> e) -> m a -> m a
  reader :: (e -> a) -> m a

instance ReaderOps e ((->) e) where
  ask e       = e
  local f x e = x (f e)
  reader f e  = f e

instance Monad m => ReaderOps e (ReaderT e m) where
  -- m a is an effectful computation producing an @a@ value
  ask       = ReaderT (\e -> pure e)
  local f g = ReaderT (\e -> runReaderT g (f e))
  reader f  = ReaderT (\e -> pure (f e))

-- Below we define reader operations on inner monads /wrapped/ by an
-- outer monad where reader operations /are/ defined. For example if
-- we have @runReader (runMaybeT ask)@, then we have built a Reader
-- computation that produces a Maybe computation (which produces the
-- value of the Reader environment).
--
-- The @ask@ in our example is delegated from @ReaderOps e (MaybeT m)@
-- up the "monad stack" to the @ask@ method in @ReaderOps e ((->) e a)@.

instance (ReaderOps e m, Monoid w) => ReaderOps e (WriterT w m) where
  ask       = lift ask
  local f m = P.error "todo"
  reader f  = lift (reader f)

instance ReaderOps e m => ReaderOps e (EitherT x m) where
  ask       = lift ask
  local f m = P.error "todo"
  reader f  = lift (reader f)

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

instance Bifunctor (,) where
  f <$$> g = \(a, b) -> (f a, g b)

instance Biplicative (,) where
  f <**> g = (fa ga, fb gb)
    where (fa, fb) = f
          (ga, gb) = g

instance (Monoid a, Monoid b) => Monoid (a, b) where
  empty  = (empty, empty)
  a <> b = (ax <> bx, ay <> by)
    where (ax, ay) = a
          (bx, by) = b

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

newtype WriterT w m a
  = WriterT { runWriterT :: m (w, a) }

instance Show (m (w, a)) => Show (WriterT w m a) where
  showsPrec p x = showParen (p > 10) (showString "WriterT " . showsPrec 11 (runWriterT x))

execWriterT :: Functor m => WriterT w m a -> m w
execWriterT x = fst <$> runWriterT x

evalWriterT :: Functor m => WriterT w m a -> m a
evalWriterT x = snd <$> runWriterT x

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
  -- | Merge the argument with the log output
  tell   :: w -> m ()

  -- | Lift a tuple of log data and value into the monad @m@
  writer :: (w, a) -> m a

instance Monoid w => WriterOps w ((,) w) where
  tell w   = (w, ())
  writer x = x

instance (Monoid w, Monad m) => WriterOps w (WriterT w m) where
  tell w   = WriterT (pure (w, ()))
  writer x = WriterT (pure x)

instance WriterOps w m => WriterOps w (ReaderT e m) where
  tell w   = P.error "todo"
  writer x = P.error "todo"

-- instance WriterOps w m => WriterOps w (EitherT x m) where
-- instance WriterOps w m => WriterOps w (StateT s m) where
-- instance WriterOps w m => WriterOps w (MaybeT m) where
-- instance WriterOps w m => WriterOps w (ListT m) where

-- Either
--------------------------------------------------------------------------------

data Either a b
  = Left  a
  | Right b
  deriving (Show, Eq)

runEither :: (a -> c) -> (b -> c) -> Either a b -> c
runEither f _ (Left a)  = f a
runEither _ f (Right b) = f b

instance Functor (Either x) where
  f <$> Left x  = Left x
  f <$> Right a = Right (f a)

instance Bifunctor Either where
  f <$$> g = runEither f' g'
    where f' a = Left (f a)
          g' b = Right (g b)

instance Applicative (Either x) where
  pure x = Right x
  Right f <*> Right x = Right (f x)
  _       <*> Left x  = Left x
  Left f  <*> _       = Left f

instance Monad (Either x) where
  f =<< Left x  = Left x
  f =<< Right a = f a

-- EitherT
--------------------------------------------------------------------------------

newtype EitherT x m a
  = EitherT { runEitherT :: m (Either x a) }

instance Show (m (Either x a)) => Show (EitherT x m a) where
  showsPrec p x = showParen (p > 10) (showString "EitherT " . showsPrec 11 (runEitherT x))

instance Functor m => Functor (EitherT x m) where
  f <$> x = EitherT ((f <$>) <$> runEitherT x)

instance Applicative m => Applicative (EitherT x m) where
  pure x  = EitherT (pure (Right x))
  f <*> x = EitherT ((<*>) <$> runEitherT f <*> runEitherT x)

instance Monad m => Monad (EitherT x m) where
  f =<< x = EitherT (runEither l r =<< runEitherT x)
    where l x = pure (Left x)
          r a = runEitherT (f a)

instance Lift (EitherT x) where
  lift m = EitherT (pure <$> m)

-- EitherOps
--------------------------------------------------------------------------------

class Monad m => EitherOps x m | m -> x where
  throw :: x -> m a
  catch :: m a -> (x -> m a) -> m a

instance EitherOps x (Either x) where
  throw x   = Left x
  catch m f = runEither f Right m

instance Monad m => EitherOps x (EitherT x m) where
  throw x   = EitherT (pure (throw x))
  catch m f = EitherT (runEither l r =<< runEitherT m)
    where l e = runEitherT (f e)
          r a = pure (Right a)

-- instance EitherOps x m => EitherOps x (ReaderT e m) where
-- instance EitherOps x m => EitherOps x (WriterT w m) where
-- instance EitherOps x m => EitherOps x (StateT s m) where
-- instance EitherOps x m => EitherOps x (MaybeT m) where
-- instance EitherOps x m => EitherOps x (ListT m) where

-- State
--------------------------------------------------------------------------------

-- type State s a
--   = StateT s Id a

newtype State s a
  = State { runState :: s -> (s, a) }

execState :: State s a -> s -> s
execState x s = fst (runState x s)

evalState :: State s a -> s -> a
evalState x s = snd (runState x s)

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

-- StateT
--------------------------------------------------------------------------------

-- Hmm! There's no nice instances to reuse!
newtype StateT s m a
  = StateT { runStateT :: s -> m (s, a) }

execStateT :: Functor m => StateT s m a -> s -> m s
execStateT x s = fst <$> runStateT x s

evalStateT :: Functor m => StateT s m a -> s -> m a
evalStateT x s = snd <$> runStateT x s

instance Functor m => Functor (StateT s m) where
  f <$> x = StateT (\s -> (f <$>) <$> runStateT x s)
  --      = StateT (((f <$>) <$>) <$> runStateT x)

instance Monad m => Applicative (StateT s m) where
  pure x  = StateT (\s0 -> pure (s0, x))
  f <*> x = StateT (\s0 -> g =<< runStateT f s0)
    where g (s1, f') = second f' <$> runStateT x s1

instance Monad m => Monad (StateT s m) where
  f =<< x = StateT (\s -> g =<< runStateT x s)
  --      = StateT ((=<<) <$> pure g <*> runStateT x)
    where g (s, a) = runStateT (f a) s

instance Lift (StateT s) where
  lift m = StateT (\s -> (\a -> (s, a)) <$> m)

-- StateOps
--------------------------------------------------------------------------------

class Monad m => StateOps s m | m -> s where
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

-- instance StateOps s m => StateOps s (ReaderT e m) where
-- instance StateOps s m => StateOps s (WriterT w m) where
-- instance StateOps s m => StateOps s (EitherT x m) where
-- instance StateOps s m => StateOps s (MaybeT m) where
-- instance StateOps s m => StateOps s (ListT m) where
