{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- Categories
-- * Hask: objects are types, morphisms are functions
-- * Hask^op: reverse all arrows in Hask
-- * Hask*Hask: functors are bifunctors
-- * Comma categories: objects are morphisms to a single object 'a',
--     and morphisms are commutative triangles
-- * Functor categories: morphisms are natural transformations
-- * Algebra categories
-- * Monoidal categories
-- * Kleisli categories


module MM
  ( Id(..)
  , Zero
  , One(..)
  , cataOne
  , (:+:)(..)
  , cataSum
  , (:*:)(..)
  , cataMul
  , Exp
  , flip
  , Const(..)
  , const
  , cataConst
  , Maybe(..)
  , some
  , none
  , cataMaybe
  , List(..)
  , null
  , cons
  , cataList
  , Functor(..)
  , Bifunctor(..)
  , Cofunctor(..)
  , Monoid(..)
  , Monad(..)
  , Category(..)
  ) where

import qualified Prelude as P

newtype Id a
  = Id { unId :: a }

-- The type inhabited by zero values
data Zero

-- The type inhabited by one value
data One
  = One

-- Substitute the constructors of one
cataOne :: a -> One -> a
cataOne a _ = a

infixl 6 :+:
infixl 7 :*:

-- Inhabited by |a| + |b| values
data (:+:) a b
  = L a
  | R b

-- Substitute the constructors of a sum
cataSum :: (a -> c) -> (b -> c) -> a :+: b -> c
cataSum finl finr x = case x of
  L a -> finl a
  R b -> finr b

-- Inhabited by |a| * |b| values
data (:*:) a b
  = (:*:) a b

-- Substitute the constructors of a mul
cataMul :: (a -> b -> c) -> a :*: b -> c
cataMul fmul x = case x of
  a :*: b -> fmul a b

-- Inhabited by |b| ^ |a| values
type Exp a b
  = a -> b

flip :: Exp a (Exp b c) -> Exp b (Exp a c)
flip f a b = f b a

-- Functors map objects and morphisms from one category to those of another, ie,
-- objects in A to objects in F(A) and morphisms between two objects A and B to
-- morphisms between F(A) and F(B).
--
-- Endofunctors are a kind of functor that map objects and morphisms from one
-- category to those of the same category. In Haskell, "Functor" maps objects
-- (types) and morphisms (functions) in Hask to ... other types and functions
-- in Hask.
--
-- The type constructor `f` maps objects. For instance, when f is `Maybe`, types
-- like `Int` and `Char` become `Maybe Int` and `Maybe Char`. Other examples are
-- `List` and `Tree`, etc.
--
-- We can also view "Functor" as a map from Hask to some subset of Hask, where
-- every object in this subset is `f Int`, `f Char`, `f ...`, and morphisms are
-- functions between these types, so this subset also forms a category.
class Functor f where
  -- identity:    fmap id = id
  -- composition: fmap g . fmap f = fmap (g . f) x
  fmap :: (a -> b) -> f a -> f b

class Bifunctor f where
  bmap :: (a -> a') -> (b -> b') -> f a b -> f a' b'

class Cofunctor f where
  cmap :: (a -> b) -> f b -> f a

-- A monad is given as an endofunctor f: C → C on a particular category, which
-- is equipped with natural transformations η: I ⇒ T and μ: T² ⇒ T. Or, in terms
-- of components, ηx: x → T(x) and μx: T²(x) → T(x).
class Functor m => Monad m where
  -- mult . unit = id
  unit :: a -> m a
  mult :: m (m a) -> m a

class Cofunctor m => Comonad m where
  unit :: m a -> a
  mult :: m a -> m (m a)

class Monoid m where
  -- left identity:  empty `assoc` x = x
  -- right identity: x `assoc` empty = x
  -- associativity:  x `assoc` (y `assoc` z) = (x `assoc` y) `assoc` z
  empty :: m
  assoc :: m -> m -> m

class Category c where
  -- left identity:  id . f = f
  -- right identity: f . id = f
  -- associativity: f . (g . h) = (f . g) . h
  id  :: c x x
  (.) :: c y z -> c x y -> c x z

newtype Const a b
  = Const { unConst :: a }

const :: a -> Const a b
const = Const

-- Substitute the constructors of a const
cataConst :: (a -> c) -> Const a b -> c
cataConst f = f . unConst

newtype Maybe a
  = Maybe { unMaybe :: a :+: One }

some :: a -> Maybe a
some = Maybe . L

none :: Maybe a
none = Maybe (R One)

-- Substitute the constructors of a maybe
cataMaybe :: (a -> b) -> b -> Maybe a -> b
cataMaybe fsome fnone maybe = case unMaybe maybe of
  L a   -> fsome a
  R One -> fnone

newtype List a
  = List { unList :: a :*: List a :+: One }

-- Empty list
null :: List a
null = List (R One)

-- Prepend an element to the front of a list
cons :: a -> List a -> List a
cons = (List . L .) . (:*:)

-- Substitute the constructors of a list
cataList :: (a -> r -> r) -> r -> List a -> r
cataList fcons fnull = cataSum finl finr . unList
  where
    finl   = cataMul (\head tail -> fcons head (cataList fcons fnull tail))
    finr _ = fnull

instance Monoid a => Monoid (Id a) where
  empty     = Id empty
  assoc a b = Id (assoc (unId a) (unId b))

instance Monoid a => Monoid (Maybe a) where
  empty     = some empty
  assoc a b = cataMaybe (\ma -> cataMaybe (some . assoc ma) none b) none a

instance Monoid (List a) where
  empty     = null              -- the empty list
  assoc a b = cataList cons b a -- concatenate two lists

instance Monoid m => Monoid (Const m b) where
  empty     = Const empty
  assoc a b = Const (assoc (unConst a) (unConst b))

instance (Monoid a, Monoid b) => Monoid (a :*: b) where
  empty                       = empty :*: empty
  assoc (a :*: b) (a' :*: b') = assoc a a' :*: assoc b b'

instance Monoid (a -> a) where
  empty     x = x
  assoc f g x = f (g x)

instance Functor Id where
  fmap f = Id . f . unId

instance Functor Maybe where
  fmap f = cataMaybe (some . f) none

instance Functor List where
  fmap f = cataList (cons . f) null

instance Bifunctor (:+:) where
  bmap f g = cataSum (L . f) (R . g)

instance Bifunctor (:*:) where
  bmap f g = cataMul (\a b -> f a :*: g b)

instance Category (->) where
  id      x = x
  (.) f g x = f (g x)

-- instance Monad Maybe where

-- instance Monad List where

