{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Bind where
import Data.Functor.Functor
import Data.Functor.Apply
import Data.Semigroupoid.Category

infixr 1 =<<

class Apply f => Bind f where
  (=<<) :: (a -> f b) -> f a -> f b
  join :: f (f a) -> f a

  (=<<) f x = join (f +$ x)
  join m = id =<< m

(>>=) :: Bind f => f a -> (a -> f b) -> f b
(>>=) x f = f =<< x

instance Bind ((->) e) where
  (=<<) f g x = f (g x) x
  join f x = f x x
