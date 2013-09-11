import qualified Data.Set as S

-- Find the first element matching a predicate
find' :: (a -> Bool) -> [a] -> Maybe a
find' p = foldr step done
  where
    step x continue
      | p x       = Just x
      | otherwise = continue
    done          = Nothing

-- Find the second element matching a predicate
find'' :: (a -> Bool) -> [a] -> Maybe a
find'' p xs = foldr step done xs False
  where
    step x continue flag
      | p x && flag   = Just x
      | p x           = continue True
      | otherwise     = continue flag
    done flag         = Nothing

-- Collect all elements matching a predicate
select' :: (a -> Bool) -> [a] -> [a]
select' p = foldr step done
  where
    step x continue
      | p x       = x : continue
      | otherwise =     continue
    done          = []

-- Collect every other element in a list
alternate' :: [a] -> [a]
alternate' xs = foldr step done xs False
  where
    step x continue flag
      | flag      = x : continue (not flag)
      | otherwise =     continue (not flag)
    done flag     = []

-- Find the index such that the sum of elements preceeding
-- equals the sum of the elements succeeding this element
middle' :: (Eq a, Num a) => [a] -> Maybe Int
middle' xs = foldr step done xs 0 0 (sum xs)
  where
    step x continue k as bs
      | as + x == bs  = Just k
      | otherwise     = continue (k+1) (as+x) (bs-x)
    done k as bs      = Nothing

-- Find the first index such that the set of elements succeeding
-- this element are a subset of the set of elements including this
-- and its preceeding elements
covering' :: Ord a => [a] -> Int
covering' xs = foldr step done xs 0 0 S.empty
  where
    step x continue k j uniq
      | x `S.member` uniq = continue k (j+1) uniq
      | otherwise         = continue j (j+1) (x `S.insert` uniq)
    done k j uniq         = k

-- Implementation of foldl in terms of foldr
-- 
--                     (:)          f
--                     / \         / \
--        z   a       a  (:)      a   f     foldr
--         \ /           / \         / \
--          f   b       b  (:)      b   f
--           \ /           / \         / \
--   foldl    f   c       c   []      c   z
--             \ /
--              f
--
foldl' :: (r -> a -> r) -> r -> [a] -> r
foldl' op z xs = foldr step done xs z
  where
    step x continue z = continue (op z x)
    done z            = z

-- Implementation of foldr in terms of foldl
-- 
--                     (:)          f
--                     / \         / \
--        z   a       a  (:)      a   f     foldr
--         \ /           / \         / \
--          f   b       b  (:)      b   f
--           \ /           / \         / \
--   foldl    f   c       c   []      c   z
--             \ /
--              f
--
foldr' :: (a -> r -> r) -> r -> [a] -> r
foldr' op z xs = foldl' step done xs z
  where
    step continue x z = continue (op x z)
    done z            = z

-- Efficiently compare the lengths of two lists, without actually
-- computing each of their lengths
compareLength' :: [a] -> [a] -> Bool
compareLength' [] []          = True
compareLength' (_:as) (_:bs)  = compareLength' as bs
compareLength' _ _            = False

-- Remove the last /n/ elements from the end of a list (and return
-- the same list if it is infinite)
dropLast :: Int -> [a] -> [a]
dropLast n xs = undefined

-- Split the given list into two halves of equal (±1) length
halve :: [a] -> ([a], [a])
halve xs = undefined

-- Moves the first /n/ elements from the given list to the end
rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs = undefined
