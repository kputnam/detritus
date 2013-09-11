{-# LANGUAGE NoImplicitPrelude #-}

module Data.Order.Count where
import Data.Semigroupoid.Monoid
import Data.Semigroupoid.Semigroup

-- newtype Count
--   = Count Integer
-- 
-- instance Semigroup Count where
--   Count n <> Count m = Count (n + m)
-- 
-- instance Monoid Count where
--   identity = Count 0
