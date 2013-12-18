{-# LANGUAGE RankNTypes #-}

import Prelude hiding (or)

-- http://www.cs.ox.ac.uk/ralf.hinze/Kan.pdf

-- Say you have implemented some computational effect using a monad, and you
-- note that your monadic program is running rather slow. There is a folklore trick
-- to speed it up: transform the monad M into continuation-passing style.

-- Hiding an 'a'
newtype C a
  = C { runC :: forall m z. (a -> m z) -> m z }

instance Monad C where
  return x  = C $ \k -> k x
  x >>= f   = C $ \k -> runC x (\a -> runC (f a) k)


-- Backtracking with a success and failure continuation
newtype B a
  = B { runB :: forall z. (a -> z -> z) -> z -> z }

no :: B a
no = B $ \_ z -> z

ok :: a -> B a
ok a = B $ \s z -> s a z

or :: B a -> B a -> B a
or m n = B $ \s z -> runB m s (runB n s z)

