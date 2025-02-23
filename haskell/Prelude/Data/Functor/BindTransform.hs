{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Functor.BindTrans where
import Data.Functor.Bind
import Data.Functor.MonadTransform

class MonadTransform t => BindTransform t where
  transformB :: (Bind f, Bind g) => (forall z. f z -> g z) -> t f a -> t g a
