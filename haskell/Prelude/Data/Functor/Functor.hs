{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Functor where

infixl 4 +$

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
  -- identity:    (+$) id = id
  -- composition: (+$) g . (+$) f = (+$) (g . f)
  (+$) :: (a -> b) -> f a -> f b

instance Functor ((->) e) where
  (+$) f g x = f (g x)
