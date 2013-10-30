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
  , Boolean(..)
  , Sum(..)
  , Product(..)
  , Min(..)
  , Max(..)
  , First(..)
  , Last(..)
  , Or(..)
  , And(..)
  , Xor(..)
  ) where

import Data.Function (on)
import qualified Prelude    as P
import qualified Data.Ratio as P ((%), numerator, denominator)
import qualified Data.Bits  as B
import qualified Data.Set   as S

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

  -- default implementation
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

-- For all elements a, b, and c
--
-- Associativity
--    a `join` (b `join` c) = (a `join` b) `join` c
--    a `meet` (b `meet` c) = (a `meet` b) `meet` c
--
-- Commutativity
--    a `join` b = b `join` a
--    a `meet` b = b `meet` a
--
-- Absorption
--    a `join` (a `meet` b) = a
--    a `meet` (a `join` b) = a
--
-- Identity
--    a `join` bottom = a
--    a `meet` top    = a
--
-- Distributivity
--    a `join` (b `meet` c) = (a `join` b) `meet` (a `join` c)
--    a `meet` (b `join` c) = (a `meet` b) `join` (a `meet` c)
--
-- Complements
--    a `meet` complement a = top
--    a `join` complement a = bottom
--
class Boolean a where
  meet        :: a -> a -> a
  join        :: a -> a -> a
  complement  :: a -> a
  bottom      :: a
  top         :: a

instance Boolean () where
  meet _ _      = ()
  join _ _      = ()
  complement _  = ()
  bottom        = ()
  top           = ()

instance Boolean P.Bool where
  meet        = (P.&&)
  join        = (P.||)
  complement  = P.not
  bottom      = P.False
  top         = P.True

----------------------------------------------------------------------------

newtype Sum a
  = Sum { getSum :: a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)
  
instance Semigroup (Sum P.Integer) where
  a + b = Sum (op a b)
    where op = (P.+) `on` getSum

instance Monoid (Sum P.Integer) where
  zero = Sum 0

instance Group (Sum P.Integer) where
  negate = Sum . P.negate . getSum
  a - b  = Sum (op a b)
    where op = (P.-) `on` getSum

instance Abelian (Sum P.Integer) where
  -- forall a b. a + b == b + a

instance Semiring (Sum P.Integer) where
  a * b = Sum (op a b)
    where op = (P.*) `on` getSum

instance Pseudoring (Sum P.Integer) where

instance Ring (Sum P.Integer) where
  one = Sum 1
  
instance Semigroup (Sum P.Rational) where
  a + b = Sum (op a b)
    where op = (P.+) `on` getSum

instance Monoid (Sum P.Rational) where
  zero = Sum 0

instance Group (Sum P.Rational) where
  negate = Sum . P.negate . getSum
  a - b  = Sum (op a b)
    where op = (P.-) `on` getSum

instance Abelian (Sum P.Rational) where

instance Semiring (Sum P.Rational) where
  a * b = Sum (op a b)
    where op = (P.*) `on` getSum

instance Pseudoring (Sum P.Rational) where

instance Ring (Sum P.Rational) where
  one = Sum 1

instance Division (Sum P.Rational) where
  a / b = Sum (op a b)
    where op = (P./) `on` getSum

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

newtype Xor a
  = Xor { getXor :: a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)

instance B.Bits a => Semigroup (Xor a) where
  a + b = Xor (B.xor (getXor a) (getXor b))

instance B.Bits a => Monoid (Xor a) where
  zero = Xor (B.clearBit (B.bit 0) 0)

instance B.Bits a => Group (Xor a) where
  negate = Xor . B.complement . getXor
  a - b  = a + b

instance B.Bits a => Abelian (Xor a) where

----------------------------------------------------------------------------

newtype Or a -- Or
  = Or { getOr :: a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)

instance B.Bits a => Semigroup (Or a) where
  a + b = Or (op a b)
    where op = (B..|.) `on` getOr

instance B.Bits a => Monoid (Or a) where
  zero = Or (B.clearBit (B.bit 0) 0)

instance B.Bits a => Semiring (Or a) where
  a * b = Or (op a b)
    where op = (B..&.) `on` getOr

----------------------------------------------------------------------------

newtype And a -- And
  = And { getAnd :: a }
  deriving (P.Eq, P.Ord, P.Read, P.Show, P.Bounded, P.Enum)

instance B.Bits a => Semigroup (And a) where
  a + b = And (op a b)
    where op = (B..&.) `on` getAnd

instance B.Bits a => Monoid (And a) where
  zero = And (B.complement (B.clearBit (B.bit 0) 0))

instance B.Bits a => Semiring (And a) where
  a * b = And (op a b)
    where op = (B..|.) `on` getAnd

----------------------------------------------------------------------------

newtype PowerSet a
  = PowerSet { getPowerSet :: S.Set (S.Set a) }
  deriving (P.Eq, P.Ord, P.Read, P.Show)

instance P.Ord a => Semigroup (PowerSet a) where
  a + b = PowerSet (op (getPowerSet a) (getPowerSet b))
    where op a b = S.union (a S.\\ b) (b S.\\ a)

instance P.Ord a => Monoid (PowerSet a) where
  zero = PowerSet S.empty

instance P.Ord a => Group (PowerSet a) where
  negate x = x

instance P.Ord a => Abelian (PowerSet a) where

instance P.Ord a => Semiring (PowerSet a) where
  a * b = PowerSet (op a b)
    where op = S.intersection `on` getPowerSet

instance P.Ord a => Pseudoring (PowerSet a) where

-- instance P.Ord a => Ring (PowerSet a) where
--   one = TODO

-- ring with (+) being symmetric difference and (*) is intersection

----------------------------------------------------------------------------

instance B.Bits P.Bool where
  a .&. b         = a P.&& b
  a .|. b         = a P.|| b
  xor P.True P.False  = P.True
  xor P.False P.True  = P.True
  xor _ _             = P.False
  complement          = P.not
  rotate x _          = x
  bit 0               = P.True
  bit _               = P.False
  setBit _ 0          = P.True
  setBit x _          = x
  clearBit _ 0        = P.False
  clearBit x _        = x
  complementBit x 0   = P.not x
  complementBit x _   = x
  testBit x 0         = x
  testBit _ _         = P.False
  bitSize _           = 1
  isSigned _          = P.False

-- Symmetric difference
-- Rubik's Cube manipulations
-- Modular arithmetic (WordN)
-- Integers
-- Polynomials
-- Functions
-- Series
-- Square matrices
-- Non-empty list
