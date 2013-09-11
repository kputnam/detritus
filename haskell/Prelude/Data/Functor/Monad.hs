{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Monad where
import Data.Functor.Applicative
import Data.Functor.Bind

-- A monad is given as an endofunctor f: C → C on a particular category, which
-- is equipped with natural transformations η: I ⇒ T and μ: T² ⇒ T. Or, in terms
-- of components, ηx: x → T(x) and μx: T²(x) → T(x).
class (Applicative f, Bind f) => Monad f where
  -- join . pure = id

instance Monad ((->) e) where
