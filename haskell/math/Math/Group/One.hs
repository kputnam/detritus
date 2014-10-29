module Math.Group.One
  where

import Math.Group
import qualified Prelude as P

-- | Set with one element
data One
  = O
  deriving (P.Eq, P.Show)

groupOne :: Group One
groupOne = Group zero (+) negate
  where
    zero     = O
    _ + _    = O
    negate _ = O
