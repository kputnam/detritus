{-# LANGUAGE DatatypeContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}

-- This example demonstrates the use of "nested types" to
-- encode invariants using the type system. The invariant
-- here is that in an AVL tree, the height of two adjacent
-- branches doesn't exceed one.
--
-- In this case, the type of a tree mirrors its shape. So
-- @branch (branch (leaf 'a') "ab" (leaf 'b')) "abc" (leaf 'c')@
-- has the type @Branch (Branch Leaf Leaf) Leaf [Char] Char@,
-- which indicates the left side is a tree with height one, and
-- the right side is a leaf (which has height zero).

data Z   = Z deriving (Eq, Show)
data S n = S n deriving (Eq, Show)

-- Not needed
-- n1 = S Z
-- n2 = S n1
-- n3 = S n2
-- n4 = S n3
-- n5 = S n4
-- 
-- type N1 = S Z
-- type N2 = S N1
-- type N3 = S N2
-- type N4 = S N3
-- type N5 = S N4

-- No runtime evidence
class Near a b
instance Near a a
instance Near (S n) n
instance Near n (S n)

-- Ordering relation on natural numbers
class Greater l r n | l r -> n where
  greater :: l -> r -> n
instance Greater Z Z Z where
  greater _ _ = Z
instance Greater Z (S n) (S n) where
  greater _ s = s
instance Greater (S n) Z (S n) where
  greater s _ = s
instance Greater l r n => Greater (S l) (S r) (S n) where
  greater (S l) (S r) = S (greater l r)

-- Height of a tree
class Height t n | t -> n where
  height :: t -> n
instance Height (Leaf k v) Z where
  height _ = Z
instance ( Height (l k v) hl
         , Height (r k v) hr
         , Greater hl hr n ) => Height (Branch l r k v) (S n) where
  height (Branch l _ r)
    = S (greater (height l) (height r))

-- This disappears!
class NearHeight l r
instance ( Height l hl
         , Height r hr
         , Near hl hr ) => NearHeight l r

data Leaf k v
  = Leaf v
  deriving (Eq, Show)

data Branch l r k v
  = Branch (l k v) k (r k v)
  deriving (Eq, Show)

-- Smart constructors
leaf :: v -> Leaf k v
leaf = Leaf

branch :: (Ord k, NearHeight (l k v) (r k v))
       => l k v -> k -> r k v -> Branch l r k v
branch l k r = Branch l k r

-- Otherwise
-- data (Ord k, NearHeight (l k v) (r k v)) => Branch l r k v
--   = Branch (l k v) k (r k v)
--
-- instance ( Show k
--          , Show v
--          , NearHeight (l k v) (r k v)
--          , Ord k
--          , Show (l k v)
--          , Show (r k v) ) => Show (Branch l r k v) where
--   showsPrec p (Branch l k r)
--     = showParen (p > 10) $
--         showString "Branch "
--       . showsPrec 11 l
--       . showString " "
--       . showsPrec 11 k
--       . showString " "
--       . showsPrec 11 r
