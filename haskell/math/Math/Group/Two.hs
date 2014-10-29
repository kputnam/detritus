module Math.Group.Two
  where

import Math.Group
import Math.Isomorphism
import Control.Applicative
import qualified Prelude as P

-- | Set with two elements
data Two
  = F
  | T
  deriving (P.Eq, P.Show)

-- | This group, along with (*) = AND, is GF(2)
groupXor :: Group Two
groupXor = Group zero (+) negate
  where
    zero     = F
    negate a = a
    F + a    = a
    T + F    = T
    T + T    = F

groupXnor :: Group Two
groupXnor = Group zero (+) negate
  where
    zero     = T
    negate a = a
    T + a    = a
    F + T    = F
    F + F    = T

-- | Isomorphism between groupXor and groupXnor
isoTwo :: Iso (Group Two) (Group Two)
isoTwo = Iso flip flip
  where
    zero' g      = not (zero g)
    times' g a b = not ((g +) a b)
    negate' g a  = negate g a

    flip  = Group <$> zero' <*> times' <*> negate'
    not T = F
    not F = T

