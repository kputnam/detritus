{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Algebra
  ( Semigroup(..)
  , Monoid(..)
  , Group(..)
  , Abelian(..)
  , Semiring(..)
  , Pseudoring(..)
  , Ring(..)
  , Division(..)
  , Field(..)
  , Semicategory(..)
  , Category(..)
  , Groupoid(..)
  , Arrow(..)

  , Sum(..)
  , Product(..)
  , Min(..)
  , Max(..)
  , First(..)
  , Last(..)
  , Any(..)
  , All(..)
  ) where

import qualified Prelude    as P
import qualified Data.Ratio as P ((%), numerator, denominator)
import Data.Function (on)

infixl 6 +
infixl 6 -
infixl 6 *
infixl 6 /

class Semigroup a where
  -- (+) is a closed associative binary operation
  --   forall x:a y:a z:a. x + (y + z) == (x + y) + z
  (+) :: a -> a -> a

class Semigroup a => Monoid a where
  -- forall x:a. zero + x == x
  -- forall x:a. x + zero == x
  zero :: a

-- http://en.wikipedia.org/wiki/Group_(mathematics)
class Monoid a => Group a where
  -- forall x:a. x + negate x == zero
  -- forall x:a. negate x + x == zero
  negate :: a -> a
  (-)    :: a -> a -> a
  a - b = a + negate b

-- http://en.wikipedia.org/wiki/Abelian_group
class Group a => Abelian a where
  -- forall x:a y:a. x + y == y + x

-- http://en.wikipedia.org/wiki/Semiring
class Monoid a => Semiring a where
  -- Ring without multiplicative identity and additive inverses
  --
  -- (a, *) is a semigroup
  --   forall a b c. a * (b * c) == (a * b) * c
  --
  -- forall x:a y:a. x + y == y + x
  --
  -- (*) distributes over (+)
  --   forall a b c. a * (b + c) == (a * b) + (a * c)
  --   forall a b c. (b + c) * a == (b * a) + (c * a)
  --
  -- forall a. zero * a = zero
  -- forall a. a * zero = zero
  (*) :: a -> a -> a

-- http://en.wikipedia.org/wiki/Pseudo-ring
class (Semiring a, Abelian a) => Pseudoring a where
  -- Ring without an multiplicative identity element
  --
  -- This automatically follows from other axioms
  --   forall a. zero * a = zero
  --   forall a. a * zero = zero

-- http://en.wikipedia.org/wiki/Ring_(mathematics)
class Pseudoring a => Ring a where
  -- one /= zero
  -- forall a. one * a == a
  -- forall a. a * one == a
  one :: a

class Ring a => Division a where
  -- forall a. a * reciprocal a == one
  -- forall a. reciprocal a * a == one
  reciprocal  :: a -> a
  (/)         :: a -> a -> a
  a / b = a * reciprocal b

class Division a => Field a where
  -- forall a b. a * b == b * a

----------------------------------------------------------------------------

class Semicategory c where
  -- forall f g h. (f . g) . h == f . (g . h)
  (.) :: c y z -> c x y -> c x z

class Semicategory c => Category c where
  -- forall f. id . f == f
  -- forall f. f . id == f
  id :: c x x

class Category c => Groupoid c where
  inverse :: c a b -> c b a
  
class Category c => Arrow c where
  -- todo

instance Semicategory (->) where
  (.) = (P..)

instance Category (->) where
  id x = x

----------------------------------------------------------------------------

newtype Sum a
  = Sum { getSum :: a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)
  
instance Semigroup (Sum P.Integer) where
  a + b = Sum (add a b)
    where add = (P.+) `on` getSum

instance Monoid (Sum P.Integer) where
  zero = Sum 0

instance Group (Sum P.Integer) where
  negate = Sum . P.negate . getSum
  a - b  = Sum (sub a b)
    where sub = (P.-) `on` getSum

instance Abelian (Sum P.Integer) where
  -- forall a b. a + b == b + a

instance Semiring (Sum P.Integer) where
  a * b = Sum (mul a b)
    where mul = (P.*) `on` getSum

instance Pseudoring (Sum P.Integer) where

instance Ring (Sum P.Integer) where
  one = Sum 1
  
instance Semigroup (Sum P.Rational) where
  a + b = Sum (add a b)
    where add = (P.+) `on` getSum

instance Monoid (Sum P.Rational) where
  zero = Sum 0

instance Group (Sum P.Rational) where
  negate = Sum . P.negate . getSum
  a - b  = Sum (sub a b)
    where sub = (P.-) `on` getSum

instance Abelian (Sum P.Rational) where

instance Semiring (Sum P.Rational) where
  a * b = Sum (mul a b)
    where mul = (P.*) `on` getSum

instance Pseudoring (Sum P.Rational) where

instance Ring (Sum P.Rational) where
  one = Sum 1

instance Division (Sum P.Rational) where
  a / b = Sum (div a b)
    where div = (P./) `on` getSum

  reciprocal a = Sum (bot a P.% top a)
    where top = P.denominator . getSum
          bot = P.numerator . getSum

instance Field (Sum P.Rational) where
  -- forall a b. a * b == b * a

----------------------------------------------------------------------------

newtype Product a
  = Product { getProduct :: a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)

----------------------------------------------------------------------------

newtype First a
  = First { getFirst :: a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)

instance Semigroup (First a) where
  a + b = a

----------------------------------------------------------------------------

newtype Last a
  = Last { getLast :: a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)

instance Semigroup (Last a) where
  a + b = b

----------------------------------------------------------------------------

newtype Min a
  = Min { getMin :: a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)

----------------------------------------------------------------------------

newtype Max a
  = Max { getMax :: a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)

----------------------------------------------------------------------------

newtype Any
  = Any { getAny :: P.Bool }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)

instance Semigroup Any where
  a + b = Any (or a b)
    where or = (P.||) `on` getAny

instance Monoid Any where
  zero = Any P.False

instance Semiring Any where
  a * b = Any (mul a b)
    where mul = (P.&&) `on` getAny

----------------------------------------------------------------------------

newtype All
  = All { getAll :: P.Bool }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)

instance Semigroup All where
  a + b = All (and a b)
    where and = (P.&&) `on` getAll

instance Monoid All where
  zero = All P.True

instance Semiring All where
  a * b = All (mul a b)
    where mul = (P.||) `on` getAll

----------------------------------------------------------------------------

-- Rubik's Cube manipulations
-- Modular arithmetic (WordN)
-- Integers
-- Polynomials
-- Functions
-- Series
-- Square matrices
-- Non-empty list
