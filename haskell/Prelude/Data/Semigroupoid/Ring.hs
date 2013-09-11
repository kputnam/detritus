{-# LANGUAGE NoImplicitPrelude #-}

module Data.Semigroupoid.Ring where
import Data.Semigroupoid.Monoid
import Data.Semigroupoid.Group

class (Monoid m, Group m) => Ring m where
  -- (R, 0, +, -) is a group  G
  -- (R, 1, *)    is a monoid M
  --
  -- commutatitivity: x + y = y + x
  -- distribution:    x * (y + z) = x * y + x * z
  --                  (x + y) * z = x * z + y * z
