{-# LANGUAGE TypeFamilies, DeriveFunctor #-}
import Control.Applicative

-- http://jozefg.bitbucket.org/posts/2013-10-21-representable-functors.html

type Hom a = (->) a
-- instance Functor Hom where
--   fmap f g = f . g

--
-- Laws:
--   toHom . toF = id
--   toF . toHom = id
type family Obj (f :: * -> *) :: *
class Functor f => Repr f where
  toHom :: f a -> Hom (Obj f) a
  toF   :: Hom (Obj f) a -> f a

lookup :: Repr f => f a -> Obj f -> a
lookup = toHom

switch :: (Repr f, Functor g) => g (f a) -> f (g a)
switch g = toF $ \o -> fmap ($ o) (fmap toHom g)

--
---------------------------------------------------------------------------

newtype Id a = Id a deriving (Eq, Show, Functor)

type instance Obj Id = ()
instance Repr Id where
  toHom (Id a) = const a
  toF f = Id (f ())

---------------------------------------------------------------------------

data Pair a = Pair a a deriving (Eq, Show, Functor)
data Two = L | R deriving (Eq, Show)

type instance Obj Pair = Two
instance Repr Pair where
  toHom (Pair a _) L = a
  toHom (Pair _ a) R = a
  toF f = Pair (f L) (f R)

---------------------------------------------------------------------------

data Forever a = Cons a (Forever a) deriving (Eq, Show, Functor)
data Nat = Z | S Nat deriving (Eq, Show)

type instance Obj Forever = Nat
instance Repr Forever where
  -- classic recursion
  toHom (Cons a _) Z      = a
  toHom (Cons _ as) (S n) = toHom as n

  -- classic co-recursion
  toF f = loop Z
    where loop n = Cons (f n) (loop (S n))

---------------------------------------------------------------------------

newtype Wrapped f a = Wrap { unWrap :: f a }

instance Repr f => Applicative (Wrap f) where
  pure    = toF . const
  f <*> a = toF $ \o -> toHom f o $ toHom a o

instance Repr f => Monad (Wrap f) where
  return  = toF . const
  m >>= f = toF $ \o -> ($ o) . toHom . f $ toHom m o
