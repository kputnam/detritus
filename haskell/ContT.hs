module ContT
  ( ContT
  , reset
  , shift
  , runContT
  ) where

import Control.Applicative

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

(>>-) :: ContT r m a -> (a -> m r) -> m r
(>>-) = runContT

instance Functor (ContT r m) where
  fmap f m = ContT $ \return_ ->
                m >>- (\a -> return_ (f a))

instance Applicative (ContT r m) where
  pure a    = ContT $ \return_ -> return_ a
  mf <*> ma = ContT $ \return_ ->
                mf >>- \f ->
                ma >>- \a ->
                return_ (f a)

instance Monad (ContT r m) where
  return a = ContT $ \return_ -> return_ a
  ma >>= f = ContT $ \return_ ->
                ma  >>- \a ->
                f a >>- \b ->
                return_ b

reset :: Applicative m => ContT r m r -> m r
reset k = k >>- pure

shift :: Applicative m => ((a -> m r) -> ContT r m r) -> ContT r m a
shift f = ContT $ \return_ -> reset (f return_)
