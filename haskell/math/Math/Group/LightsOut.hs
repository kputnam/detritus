module Math.Group.LightsOut
  where

import Math.Group
import Math.Group.Two
import qualified Prelude     as P
import qualified Data.Matrix as X

-- One application of groupMat using GF(2) is to model the game
-- Lights Out. The game state can be modeled in the obvious way,
-- each light in the 5 x 5 grid is either on (T) or off (F). The
-- buttons which toggle lights in the game can also be modeled
-- using `zero = F` for no-change and `negate zero = T` otherwise.
button :: (P.Int, P.Int) -> X.Matrix Two
button p = X.matrix 5 5 effect
  where
    distance (a,m) (b,n) = P.abs (a P.- b) P.+ P.abs (m P.- n)
    effect q | p P.== q            = T
             | distance p q P.== 1 = T
             | P.otherwise         = F

-- Notice the following properties
-- * It doesn't matter which order we push the buttons because
--   addition in GF(2) is commutative: a + b = b + a
-- * Pushing a button twice cancels out the first: a + a = 0
--
-- From a given start state can we find the set of buttons that
-- sum to 0, representing all lights turned off? start + a + b
-- + c + ... = 0 can also be written as a + b + c + ... = S. So
-- generally, which subset of Group (X.Matrix Two) adds to S?
--
-- S = a * button (1,1)
--   + b * button (1,2)
--   + c * button (1,3)
--   + d * button (1,4)
--   + e * button (1,5)
--   + f * button (2,1)
--   + g * button (2,2)
--   + h * button (2,3)
--   + i * button (2,4)
--   + j * button (2,5)
--   + k * button (3,1)
--   + l * button (3,2)
--   + m * button (3,3)
--   + n * button (3,4)
--   + o * button (3,5)
--   + p * button (4,1)
--   + q * button (4,2)
--   + r * button (4,3)
--   + s * button (4,4)
--   + t * button (4,5)
--   + u * button (5,1)
--   + v * button (5,2)
--   + w * button (5,3)
--   + x * button (5,4)
--   + y * button (5,5)
