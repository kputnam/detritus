{-# LANGUAGE FunctionalDependencies #-}

module Bool
  where

data T = T deriving (Show, Eq)
data F = F deriving (Show, Eq)

class Not a b | a -> b where
  not :: a -> b

instance Not T F where
  not T = F

instance Not F T where
  not F = T

-- not :: Not a b => a -> b
-- not T = F
-- not F = T
