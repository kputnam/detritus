module L
  ( L
  , empty
  , single
  , wrap
  , unwrap
  , cons
  , append
  , map
  , foldl
  , foldr
  ) where

import Prelude hiding (map, foldl, foldr)

-- Bill's notes:
--   https://gist.github.com/zot/5359833
--
-- Real World Haskell
--   http://book.realworldhaskell.org/read/data-structures.html#data.dlist
--
-- Left-assocative concatenation
--   ((([] ++ as) ++ bs) ++ cs) ++ ds
--
-- Given (++) is O(n) in the length of the first
--   argument, left-associative concatenation is
--   O(n^2) in the length of the final list. You
--   can see the prefix of the list grows at each
--   step, so we have O(|as|) + O(|as|+|bs|) +
--   O(|as|+|bs|+|cs|) + etc.
--
-- Right-associative concatenation
--   as ++ (bs ++ (cs ++ (ds ++ [])))
--
-- However, if we change the order in which we
--   concatenate, this ensures the same prefix
--   is never traversed more than once. Remember
--   (++) is linear in the length of the *first*
--   arg, which is now O(|as|) + O(|bs|) +
--   O(|cs|) + etc. Concatenating lists this
--   way is O(n), instead of O(n^2).
--
-- Difference lists are a representation that
--   defers construction of the final list to
--   provide constant-time appending. Perhaps
--   the name "difference list" was chosen
--   because it stores only the *difference*
--   between the prefix and the rest of the
--   list. That is the representation of [1,2,3]
--   is (\rest -> [1,2,3] ++ rest).

-- L is a type alias for functions [a] -> [a]
type L a = [a] -> [a]

-- O(1) Convert an ordinary linked list to L,
-- by appending the list to a yet-to-be-known
-- "end of the list"
wrap :: [a] -> L a
wrap as = \rest -> as ++ rest

-- O(n) Convert an L to an ordinary list, by
-- filling in the "end of the list" to be an
-- empty list. Note:
--   wrap (unwrap l) == l
--   unwrap (wrap a) == a
unwrap :: L a -> [a]
unwrap as = as []

-- O(1) Construct an empty L, which simply
-- returns the "end of the list" without
empty :: L a
empty = \rest -> rest

-- O(1) Construct a single-element L, which
-- prepends the element to the "end of the list"
single :: a -> L a
single x = \rest -> x : rest

-- O(1) Prepend a single element to an L, by
-- constructing and appending a singleton L to
-- the given L
cons :: a -> L a -> L a
cons a as = append (single a) as

-- O(1) Combine two Ls. We reify the second
-- list (by passing it the "end of the list"),
-- then pass the result as a new "end of the
-- list" to the first list.
--
-- Note: f(g(x)) = f . g, so we can write
--  append as bs = as . bs
--        append = (.)
--
append :: L a -> L a -> L a
append as bs = \rest -> as (bs rest)

-- Example usage:
--    let as = wrap "hai"
--        bs = wrap "bub"
--        cs = wrap "mom"
--        ds = wrap "dad"
--        xx = as `append` bs `append` cs `append` ds
--     in unwrap xx
--
-- Produces "haibubmomdad"


-- Improve your understanding by implementing the standard
-- functions on [a], but for L a:

foldr :: (a -> b -> b) -> b -> L a -> b
foldr op z as = undefined

foldl :: (b -> a -> b) -> b -> L a -> b
foldl op z as = undefined

map :: (a -> b) -> L a -> L b
map f as = undefined

