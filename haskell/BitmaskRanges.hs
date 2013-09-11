import Data.List
import Data.Bits

isSucc :: (Eq a, Enum a) => a -> a -> Bool
isSucc x y = y == succ x

ranges :: (Eq a, Enum a, Bits a) => a -> Int -> [(a,a)]
ranges mask size = runs (matches strings)
  where
    strings = [xor mask mask .. bit size]
    matches = filter ((mask ==) . (.&. mask))

runs :: (Eq a, Enum a) => [a] -> [(a,a)]
runs []     = []
runs (x:xs) = go [] x x xs
  where
    go acc a b []   = reverse ((a,b):acc)
    go acc a b (c:ds)
      | isSucc b c  = go acc a c ds
      | otherwise   = go ((a,b):acc) c c ds
