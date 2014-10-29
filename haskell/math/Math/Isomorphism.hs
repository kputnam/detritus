module Math.Isomorphism
  where

-- Isomorphism between a and b
data Iso a b
  = Iso
  { fw :: a -> b
  , bw :: b -> a }
