module Math.Ring
  where

data Ring a
  = Ring
  { zero   :: m
  , one    :: m
  , (+)    :: m -> m -> m
  , (*)    :: m -> m -> m
  , negate :: m -> m }
