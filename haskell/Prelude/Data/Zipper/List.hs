{-# LANGUAGE NoImplicitPrelude #-}

module Data.Zipper.List where
import Data.Functor.Functor
import Data.Functor.Extend
import Data.Functor.Comonad
import Data.List
import Data.Functor.Applicative

data ZList a
  = ZList (List a) a (List a)

instance Functor ZList where
  f +$ ZList as x bs = ZList (f +$ as) (f x) (f +$ bs)

instance Extend ZList where
--extend :: (ZList a -> b) -> ZList a -> ZList b
  extend f (ZList as x bs) = ZList as' x' bs'
    where
      x'  = f (ZList as x bs)
      as' = as'
      bs' = bs'
      zap = zip +$ inits <*> tails

instance Comonad ZList where
  extract (ZList _ x _) = x
