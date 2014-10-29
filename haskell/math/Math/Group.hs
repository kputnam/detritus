module Math.Group
  where

import qualified Prelude as P

-- a + (b + c) = (a + b) + c
-- a + zero = zero + a = a
-- a + negate a = zero
data Group m =
  Group
  { zero   :: m
  , (+)    :: m -> m -> m
  , negate :: m -> m }

(-) :: Group m -> m -> m -> m
(-) g a b = (g +) a (negate g b)
