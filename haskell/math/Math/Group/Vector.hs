module Math.Group.Vector
  where

import Math.Group
import Control.Applicative
import qualified Prelude     as P
import qualified Data.Matrix as X
import qualified Data.Vector as V

-- | Construct a group whose elements are vectors of the given
--   dimension with elements drawn from the given group
groupVec :: Group a -> P.Int -> Group (V.Vector a)
groupVec g m = Group zero' (++) negate'
  where
    zero'     = V.replicate m (zero g)
    negate' a = negate g <$> a
    a ++ b    = V.zipWith (g +) a b

-- One application of groupVec using GF(2) is "perfect secrecy"
-- cryptography. While a secret key K and plain text P can be
-- chosen from any field, and encrypted by adding them together
-- to construct ciphertext C=P+K, and decrypted using K: C-K=P,
-- GF(2) is special.
--
-- Choosing a uniformly random secret key guarantees uniformly
-- random ciphertext, independent from the plaintext. This means
-- observing the ciphertext provides no information about the
-- plaintext.
--
-- The secret key K can also be constructed from some number of
-- uniformly random keys, K = K1 + K2 + K3. Knowing parts of the
-- key yields no information about the entire key. Parts of the
-- key could be stored on different servers, given to different
-- members of a team, or hidden in different physical locations.

-- | Construct a group whose elements are matrices of the given dimension
--   with elements drawn from the given group
groupMat :: Group a -> P.Int -> P.Int -> Group (X.Matrix a)
groupMat g m n = Group zero' (++) negate'
  where
    zero'     = X.matrix m n (\_ -> zero g)
    negate' a = negate g <$> a
    a ++ b    = X.matrix m n ((g +) <$> (a X.!) <*> (b X.!))

