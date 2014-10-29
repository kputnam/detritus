module Math.Group.Permutation
  where

import Math.Group
import qualified Prelude     as P
import qualified Data.Maybe  as P
import qualified Data.Map    as M

-- | A permutation rearranges elements of a set
data Permutation a
  = P { permute :: a -> a
      , unpmute :: a -> a }

-- | Construct a permutation function and its inverse
fromList :: P.Ord a => [(a, a)] -> Permutation a
fromList xs = P permute unpmute
  where
    permute     = assoc xs
    unpmute     = assoc (P.map swap xs)
    swap (a, b) = (b, a)
    assoc ys x  = P.fromMaybe x (M.lookup x h)
      where h = M.fromList ys

-- | Construct a group whose elements are permutation functions
groupPerm :: Group (Permutation a)
groupPerm = Group zero (+) negate
  where
    zero           = P (\x -> x) (\x -> x)
    negate (P p u) = P u p

    -- | Function composition
    P p u + P p' u'  = P permute' unpmute'
      where permute' x = p' (p x)
            unpmute' x = u (u' x)
