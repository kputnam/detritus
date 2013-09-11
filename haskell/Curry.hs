{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module PropositionalEquality where

data Nat
  = Zero
  | Succ Nat

data Vec :: * -> Nat -> * where
  VNil  :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

safeHead :: Vec a (Succ n) -> a
safeHead (VCons a _) = a

safeTail :: Vec a (Succ n) -> Vec a n
safeTail (VCons _ v) = v

-- Stupid warning about unmatched pattern (VCons _ VNil)
safeCadr :: Vec a (Succ (Succ n)) -> a
safeCadr (VCons _ (VCons a _)) = a

boolEq :: Nat -> Nat -> Bool
boolEq Zero     Zero     = True
boolEq (Succ a) (Succ b) = boolEq a b
boolEq _        _        = False

data SBool :: Bool -> * where
  STrue  :: SBool True
  SFalse :: SBool False

type family BoolEq (a :: Nat) (b :: Nat) :: Bool
type instance BoolEq Zero Zero         = True
type instance BoolEq (Succ a) (Succ b) = BoolEq a b
type instance BoolEq Zero (Succ x)     = False
type instance BoolEq (Succ x) Zero     = False

data PropEq :: k -> k -> * where
  Refl :: PropEq x x

boolToProp :: (BoolEq a b ~ True) => PropEq a b
boolToProp = Refl

--cadr :: Vec a n -> a
cadr :: SBool (BoolEq n (Succ (Succ n'))) => a -> Vec a n -> a
cadr evidence d v = case evidence of
  STrue  -> safeHead (safeTail v)
  SFalse -> d

