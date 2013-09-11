{-# LANGUAGE NoImplicitPrelude #-}

module Data.Order.Eq where
import Data.Bool
import Data.Semigroupoid.Semigroupoid

infix 4 ==
infix 4 /=

class Eq a where
  (==) :: a -> a -> Bool
  (==) = (not .) . (/=)

  (/=) :: a -> a -> Bool
  (/=) = (not .) . (==)

instance Eq Bool where
  True  == True  = True
  False == False = True
  _ == _         = False
