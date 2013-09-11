import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import Data.Foldable    as F
import Data.Traversable as T
import Data.Void

-- https://www.fpcomplete.com/user/edwardk/bound

newtype Scope f a
  = Scope { runScope :: f (Maybe a) }
  deriving (Functor, Traversable, Foldable)

instance Monad f => Monad (Scope f) where
  return        = Scope . return . Just
  Scope m >>= f =

instance MonadTrans Scope where
  lift = Scope . liftM Just

abstract :: (Functor f, Eq a) => a -> f a -> Scope f a
abstract x xs = Scope (fmap go xs)
  where
    go y = y <$ guard (x /= y)

instantiate :: Monad f => f a -> Scope f a -> f a
instantate x (Scope xs) = xs >>= go
  where
    go Nothing  = x
    go (Just y) = return y

data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Lam (Scope Exp a)
  deriving (Functor, Foldable, Traversable)

instance Monad Exp where
  return        = Var
  Var a   >>= f = f a
  App g x >>= f = App (g >>= f) (x >>= f)
  Lam e   >>= f = Lam (e >>= lift . f)

