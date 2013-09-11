module Huffman
  ( build
  , fromTree
  , toTree
  , encode
  , decode
  -- Internals
  , table
  , index
  , encodeWith
  , decodeWith
  ) where

import qualified Data.Map as M
import Control.Monad (join)
import Data.Function (on)
import Control.Arrow ((&&&))
import Control.Applicative

data Bit
  = L
  | R
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

data Tree a
  = Leaf Int a
  | Fork Int [a] (Tree a) (Tree a)
  deriving (Show, Read, Eq)

data Codec a = Codec { encode :: [a] -> Maybe [Bit]
                     , decode :: [Bit] -> Maybe [a]
                     , toTree :: Tree a }

build :: Ord a => [a] -> Maybe (Codec a)
build items = fromTree <$> index items

fromTree :: Ord a => Tree a -> Codec a
fromTree t = Codec (encodeWith t) (decodeWith t) t

encodeWith :: Ord a => Tree a -> [a] -> Maybe [Bit]
encodeWith t as = join <$> (sequence $ map (flip M.lookup t') as)
  where t' = table t

decodeWith :: Tree a -> [Bit] -> Maybe [a]
decodeWith t bs = aux t bs [] True
  where
    aux (Leaf _ c) xs acc _         = aux t xs (c:acc) True
    aux (Fork _ _ l _) (L:xs) acc _ = aux l xs acc False
    aux (Fork _ _ _ r) (R:xs) acc _ = aux r xs acc False
    aux _ [] acc True               = Just (reverse acc)
    aux _ [] _ False                = Nothing

table :: Ord a => Tree a -> M.Map a [Bit]
table t = aux t M.empty
  where
    aux (Fork _ _ l r) acc = merge (aux l acc) (aux r acc)
    aux (Leaf _ c)     acc = M.insert c [] acc
    merge l r              = M.union ((L:) <$> l) ((R:) <$> r)

index :: Ord a => [a] -> Maybe (Tree a)
index = fold . fromList weight . leaves . freq
  where
    freq    = M.toList . foldl inc M.empty
    inc m k = M.alter (Just . maybe 1 (1 +)) k m
    leaves  = map $ uncurry (flip Leaf)

    weight (Leaf n _)     = n
    weight (Fork n _ _ _) = n

    values (Leaf _ x)      = [x]
    values (Fork _ xs _ _) = xs

    merge l r  = Fork (weight' l r) (values' l r) l r
      where weight' = (+)  `on` weight
            values' = (++) `on` values

    fold :: PQ (Tree a) -> Maybe (Tree a)
    fold pq = do { (l, pr) <- minView pq
                 ; (r, ps) <- minView pr
                 ; fold $ insertWith weight ps $ merge r l
                 } <|> fst <$> minView pq

newtype PQ a = PQ (M.Map Int [a])

fromList :: (a -> Int) -> [a] -> PQ a
fromList f = PQ . M.fromListWith (++) . map (f &&& (:[]))

insertWith :: (a -> Int) -> PQ a -> a -> PQ a
insertWith f (PQ items) x = PQ items'
  where items' = M.insertWith (++) (f x) [x] items

minView :: PQ a -> Maybe (a, PQ a)
minView (PQ items)
  | M.null items = Nothing
  | otherwise    = case M.findMin items of
                     (_,   []) -> error "impossible"
                     (k,  [a]) -> Just (a, PQ $ M.delete k items)
                     (k, a:as) -> Just (a, PQ $ M.insert k as items)
