{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Fact where

data Zero
data Succ n

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

zero  :: Zero
zero  = undefined

one   :: One
one   = undefined

two   :: Two
two   = undefined

three :: Three
three = undefined

four  :: Four
four  = undefined

class Reify a where
  reify :: a -> Int

instance Reify Zero where
  reify _ = 0

instance Reify n => Reify (Succ n) where
  reify m = 1 + reify (prev m)

class Prev a b | a -> b where
  prev :: a -> b

instance Prev Zero Zero where
  prev = undefined

instance Prev (Succ n) n where
  prev = undefined

class Add a b c | a b -> c where
  add :: a -> b -> c
  
instance Add  Zero b b where
  add = undefined

instance Add a b c => Add (Succ a) b (Succ c) where
  add = undefined

class Mul a b c | a b -> c where
  mul :: a -> b -> c

instance Mul Zero b Zero where
  mul = undefined

instance (Mul a b c, Add b c d) => Mul (Succ a) b d where
  mul = undefined

class Fac a b | a -> b where
  fac :: a -> b

instance Fac Zero One where
  fac = undefined

instance (Fac n k, Mul (Succ n) k m) => Fac (Succ n) m where
  fac = undefined

-- reify (fac four)
