-- Previously, on bound the capture-avoiding substitution slayer
--
-- data Exp a
--   = Var a
--   | App (Exp a) (Exp a)
--   | Lam (Exp (Maybe a))


-- Normally working with De Bruijn terms means you traverse all
-- the way down to the leaves to increment. However, this form
-- allows incrementing the entire tree at once. The upshot is
-- lift an expression that doesn't have our bound term into the
-- new Scope no longer requires touching every leaf.
data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Lam (Exp (Maybe (Exp a)))

newtype Scope f a
  = Scope { runScope :: f (Maybe (f a)) }
  deriving (Functor, Foldable, Traversable)

instance Monad f => Monad (Scope f) where

instance MonadTrans Scope where

abstract :: (Monad f, Eq a) => a -> f a -> Scope f a

instantiate :: Monad f => f a -> Scope f a -> f a
