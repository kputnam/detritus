{-# LANGUAGE NoImplicitPrelude #-}

module Data.Bool
  ( Bool(..)
  , (&&)
  , (||)
  , not
  , xor
  , if_
  , Any
  , All
  ) where
import Data.Semigroupoid.Semigroup
import Data.Semigroupoid.Monoid

data Bool
  = True
  | False

infixr 3 &&
(&&) :: Bool -> Bool -> Bool
True  && a = a
False && _ = False

infixr 2 ||
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || a = a

not :: Bool -> Bool
not True  = False
not False = True

xor :: Bool -> Bool -> Bool
xor False b = b
xor True  b = not b

if_ :: Bool -> a -> a -> a
if_ True a _  = a
if_ False _ b = b

newtype All
  = All Bool

instance Semigroup All where
  All a <> All b = All (a && b)

instance Monoid All where
  identity = All True

newtype Any
  = Any Bool

instance Semigroup Any where
  Any a <> Any b = Any (a || b)

instance Monoid Any where
  identity = Any False
