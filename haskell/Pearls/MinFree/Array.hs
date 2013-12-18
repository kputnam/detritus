module MinFree.Array ( minFree, linearSort ) where

import Data.Array
import Data.Word

type Nat = Word32

-- Find the smallest number not in the given list of finite numbers
minFree :: [Nat] -> Nat
minFree = search . countItems

search :: Array Nat Nat -> Nat
search = fromIntegral . length . takeWhile (/= 0) . elems

countItems :: [Nat] -> Array Nat Nat
countItems xs = accumArray (+) 0 (0, n) (zip xs' (repeat 1))
  where n   = fromIntegral $ length xs
        xs' = filter (<= n) xs

-- Bonus
-- linearSort :: [Nat] -> [Nat]
linearSort xs = concat [ replicate (fromIntegral k) x
                       | (x, k) <- assocs $ countItems xs ]
