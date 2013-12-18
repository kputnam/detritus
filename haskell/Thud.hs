import Control.Monad
import Control.Applicative

data Thud a
  = Thud

instance Functor Thud where
  fmap f Thud = Thud

instance Applicative Thud where
  pure _        = Thud
  Thud <*> Thud = Thud

instance Monad Thud where
  return _    = Thud
  Thud >>= _  = Thud
