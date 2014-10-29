{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies  #-}

-- http://fho.f12n.de/posts/2014-05-07-dont-fear-the-cat.html

import Data.Functor.Foldable
import Data.List (splitAt, unfoldr)

data TreeF c f
  = EmptyF
  | LeafF c
  | NodeF f f
  deriving (Eq, Show, Functor)

-- | Co-algebra
unflatten :: [a] -> TreeF a [a]
unflatten []  = EmptyF
unflatten [x] = LeafF x
unflatten xs  = NodeF l r
  where (l, r) = splitAt (length xs `div` 2) xs

-- Algebra
flatten :: Ord a => TreeF a [a] -> [a]
flatten EmptyF      = []
flatten (LeafF x)   = [x]
flatten (NodeF l r) = mergeLists l r

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists = curry $ unfoldr c where
  c ([], [])     = Nothing
  c ([], y:ys)   = Just (y, ([], ys))
  c (x:xs, [])   = Just (x, (xs, []))
  c (x:xs, y:ys)
    | x <= y     = Just (x, (xs, y:ys))
    | otherwise  = Just (y, (x:xs, ys))

mergeSort :: Ord a => [a] -> [a]
mergeSort = hylo flatten unflatten
