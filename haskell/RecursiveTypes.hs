module Recursion where

fix :: (a -> a) -> a
fix f = f (fix f)

newtype Fix f
  = Roll { unRoll :: f (Fix f) }

-- Fold over arbitrary recursive types
fold :: Functor f => (f a -> a) -> Fix f -> a
fold h = h . fmap (fold h) . unRoll






data ListF a r
  = Nil
  | Cons a r

type List a
  = Fix (ListF a)

instance Functor (ListF a) where
  fmap _ Nil        = Nil
  fmap f (Cons a r) = Cons a (f r)

nil :: List a
nil = Roll Nil

cons :: a -> List a -> List a
cons a r = Roll (Cons a r)

example :: List Char
example = cons 'a' (cons 'b' (cons 'c' nil))

foldL :: (ListF a r -> r) -> Fix (ListF a) -> r
foldL = fold
