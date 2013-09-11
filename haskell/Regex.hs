module Regex
  ( match
  , prettyString
  , prettyShow
  -- Public constructors
  , token
  , string
  , option
  , oneof
  , kleene
  , either
  , concat
  ) where

import Prelude hiding (either, concat)
import Data.Monoid
import Control.Applicative

data Regex a
  = Null                        -- matches nothing
  | Epsilon                     -- ""
  | Token a
  | Concat (Regex a) (Regex a)  -- RR
  | Union (Regex a) (Regex  a)  -- R|R
  | Kleene (Regex a)            -- R*
  deriving (Eq, Show, Read)

-- parse (f <$> r) as == parse r (f <$> as)
instance Functor Regex where
  fmap _ Null         = Null
  fmap _ Epsilon      = Epsilon
  fmap f (Token a)    = Token (f a)
  fmap f (Concat a b) = Concat (fmap f a) (fmap f b)
  fmap f (Union a b)  = Union (fmap f a) (fmap f b)
  fmap f (Kleene a)   = Kleene (fmap f a)

prettyString :: Regex Char -> String
prettyString = p
  where
    p Null         = "∅"
    p Epsilon      = "ε"
    p (Token a)    = [a]
    p (Concat a b) = p a <> p b
    p (Union a b)  = "(?:" <> p a <> "|" <> p b <> ")"
    p (Kleene a)   = "(?:" <> p a <> ")*"

prettyShow :: Show a => Regex a -> String
prettyShow = p
  where
    p Null         = "∅"
    p Epsilon      = "ε"
    p (Token a)    = show a
    p (Concat a b) = p a <> p b
    p (Union a b)  = "(?:" <> p a <> "|" <> p b <> ")"
    p (Kleene a)   = "(?:" <> p a <> ")*"

token  :: a -> Regex a
token  = Token

option :: Regex a -> Regex a
option = flip Union Epsilon

kleene :: Regex a -> Regex a
kleene = Kleene

either :: Regex a -> Regex a -> Regex a
either = Union

concat :: Regex a -> Regex a -> Regex a
concat = Concat

match :: Eq a => Regex a -> [a] -> Bool
match r     [] = nullable r
match r (a:as) = match (derive r a) as

string :: [a] -> Regex a
string []     = Epsilon
string [a]    = Token a
string (a:as) = foldl (\l r -> Concat l (Token r)) (Token a) as

oneof :: [Regex a] -> Regex a
oneof []     = Epsilon
oneof [a]    = a
oneof (a:as) = foldl Union a as

nullable :: Regex a -> Bool
nullable Epsilon      = True
nullable Null         = False
nullable (Token _)    = False
nullable (Concat a b) = nullable a && nullable b
nullable (Union a b)  = nullable a || nullable b
nullable (Kleene _)   = True

derive :: Eq a => Regex a -> a -> Regex a
derive Epsilon      = pure Null
derive Null         = pure Null
derive (Token c')   = if_    <$> (c' ==)  <~> Epsilon <~> Null
derive (Union a b)  = Union  <$> derive a <*> derive b
derive (Kleene a)   = Concat <$> derive a <~> Kleene a
derive (Concat a b)
  | nullable a      = Union  <$> (Concat <$> derive a <~> b) <*> (derive b)
  | otherwise       = Concat <$> derive a <~> b


-- Junk
-----------------------------------------

if_ :: Bool -> a -> a -> a
if_ c t f = if c then t else f

infixl 4 <~>
(<~>) :: Applicative f => f (a -> b) -> a -> f b
(<~>) f a = f <*> pure a
