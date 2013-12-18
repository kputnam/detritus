module MinFree.Array ( minFree ) where

import Data.List
import Data.Word

type Nat = Word32

-- Find the smallest number not in the given list of finite numbers
minFree :: [Nat] -> Nat
minFree xs = head ([0..] \\ xs)
