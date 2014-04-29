{-# LANGUAGE NoImplicitParams #-}
{-# LANGUAGE TypeOperators    #-}

module Data.Semigroupoid.Split where
import Data.Semigroupoid.Category
import Data.Semigroupoid.ChooseOut
import Data.Semigroupoid.CombineOut
import Data.Semigroupoid.Left
import Data.Semigroupoid.Right

class (Category m, Left m, Right m, CombineOut m, ChooseOut m) => Split m where
  out :: (a -> b) -> m b a
