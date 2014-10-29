module Math.VectorSpace
  where

import qualified Prelude      as P
import qualified Data.Complex as C
import Math.Field

-- A vector space over the set m and field n
--
-- zero is the additive identity
--   zero + a = a + zero = a
--
-- addition is commutative
--   a + b = b + a
--
-- scale distributes over addition
--   a + scale k b = scale k (a + b)
--
data VectorSpace m n =
  VectorSpace
  { field :: Field n
  , zero  :: m
  , scale :: n -> m -> m
  , (+)   :: m -> m -> m }

vsComplexInt :: P.RealFloat a => VectorSpace (C.Complex a) P.Int
vsComplexInt = VectorSpace field zero scale (+)
  where
    field     = P.undefined
    zero      = 0
    scale k c = P.fromIntegral k P.* c
    a + b     = a P.+ b
