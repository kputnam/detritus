
-- Isomorphic to Either
data Var b a
  = B b -- bound
  | F a -- free
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Monad (Var b) where
  return    = F
  F a >>= f = f a
  B b >>= _ = B b

newtype Scope b f a
  = Scope { runScope :: f (Var b (f a)) }
  deriving (Functor, Foldable, Traversable)

instance Monad f => Monad (Scope b f) where

instance MonadTrans (Scope b) where

abstract :: Monad f => (a -> Maybe b) -> f a -> Scope b f a

instantiate :: Monad f => (b -> f a) -> Scope b f a ->  f a

data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Abs (Scope () Exp a)                    -- not clear why this has no bound vars
  | Let [Scope Int Exp a] (Scope Int Exp a) -- somehow deals with recursive bindings?
  deriving (Functor, Foldable, Traversable)

instance Monad Exp where


