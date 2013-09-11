import Control.Applicative
import Data.Monoid
import Data.Foldable    as F
import Data.Traversable as T
import Data.Void

-- https://www.fpcomplete.com/user/edwardk/bound

newtype Scope f a
  = Scope (f a)
  deriving (Eq, Show)

data Expr a
  = F a                     -- free variable
  | B Int                   -- variable bound /n/ binders up
  | App (Expr a) (Expr a)
  | Abs (Scope Expr a)      -- no need to declare which variable is bound
  deriving (Eq, Show)

abstract :: Eq a => a -> Expr a -> Scope Expr a
abstract me expr = Scope (letmeB 0 expr)
  where
    letmeB this (F you) | you == me = B this  -- capture a free variable
                        | otherwise = F you
    letmeB this (B that)            = B that  -- never touch bound variables
    letmeB this (App fun arg)       = letmeB this fun `App` letmeB this arg
    letmeB this (Abs (Scope body))  = letmeB (succ this) body

instantiate :: Expr a -> Scope Expr a -> Expr a
instantiate what (Scope body) = what'sB 0 body
  where
    what'sB this (B that) | this == that = what     -- substitute bound variable
                          | otherwise    = B that
    what'sB this (F you)                 = F you    -- never substitute free variables
    what'sB this (App fun arg)           = what'sB this fun `App` what'sB this arg
    what'sB this (Abs (Scope body))      = what'sB (succ this) body

-- Pros
-- 1. it's hard to screw up while walking under binders
-- 2. alpha-equivalence is easy with De Bruijn indices
-- 3. we can define a monad that does capture avoiding substitution
-- 4. We can use Traversable and Foldable to find free variables and closed terms

instance Functor Expr where
  -- Rename free variables (preserving their freedom)
  fmap f (F a)           = F (f a)
  fmap _ (B a)           = B a
  fmap f (App g x)       = App (fmap f g) (fmap f x)
  fmap f (Abs (Scope e)) = Abs $ Scope $ fmap f e

instance Monad Expr where
  return = F

  -- Substitute free variables
  F a >>= f           = f a
  B n >>= _           = B n
  App g x >>= f       = App (g >>= f) (x >>= f)
  Abs (Scope e) >>= f = Abs $ Scope $ e >>= f

instance Traversable Expr where
  -- :: Applicative f => (a -> f b) -> Expr a -> f (Expr b)
  traverse f (F a)           = F <$> f a
  traverse f (B n)           = pure (B n)
  traverse f (App g x)       = App <$> traverse f g <*> traverse f x
  traverse f (Abs (Scope e)) = Abs . Scope <$> traverse f e

closed :: Expr a -> Maybe (Expr Void)
closed a = (undefined <$) <$> traverse (const Nothing) a

isClosed :: Expr a -> Bool
isClosed = F.all (const False)

instance Foldable Expr where
  -- :: Monoid m => (a -> m) -> Expr a -> m
  foldMap f (F a)           = f a
  foldMap f (B n)           = mempty
  foldMap f (App g x)       = foldMap f g <> foldMap f x
  foldMap f (Abs (Scope e)) = foldMap f e

freeVars :: Expr a -> [a]
freeVars = foldMap (:[])

-- Cons
-- 1. Lots of succage (each time we descend into a lambda body)
-- 2. Still can represent illegal terms like Abs (Scope (B 2))
-- 3. We have to define ad-hoc implementations of abstract and
--    instantiate for each different expression type.
