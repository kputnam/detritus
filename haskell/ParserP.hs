{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FunctionalDependencies #-}

module Parser
  ( Position
  , Input
  , Partial(..)
  , Answer(..)
  , Result(..)
  , Feedable(..)
  , Parser(..)
--( offset
--, column
--, line
--, advance
----
--, parse
--, parseOnly
--, feed
--, (<?>)
--, done
--, position
--, eof
--, char
--, notChar
--, anyChar
--, satisfy
--, skip
--, skipWhile
--, satisfyWith
--, take
--, takeWith
--, takeRest
--, text
--, (.*>)
--, (<*.)
--, takeWhile
--, takeTill
--, manyTill
--, takeWhile1
--, skipSpace
--, skipSpace1
  ) where

import Prelude hiding (take, takeWhile)

import Control.Applicative
import Control.Monad
import Data.Monoid

-- import qualified Data.Text as T
-- import qualified Data.Char as C

data Partial t a
  = Done a
  | More (t -> Partial t a)

instance Show a => Show (Partial t a) where
  showsPrec p (More _) = case compare p 10 of
    GT -> ("(More _)" ++)
    _  -> ("More _"   ++)
  showsPrec p (Done a) = case compare p 10 of
    GT -> (("(Done " <> showsPrec 11 a ")") <>)
    _  -> ("Done " <>) . showsPrec 11 a

  showList (h:t) s
    = '[' : showsPrec (-1) h ss
    where
      ts = map (precomma . showsPrec (-1)) t
      ss = foldr (.) (<> s) ts "]"
      precomma = ((',' :) .)
  showList ps s
    = '[' : foldr (.) (<> s) qs "]"
    where
      qs = map (showsPrec (-1)) ps

instance Functor (Partial t) where
  fmap f (Done a) = Done (f a)
  fmap f (More g) = More (fmap f . g)

instance Applicative (Partial t) where
  pure = Done
  Done g <*> Done b = Done (g b)
  Done g <*> More b = More $ (g <$>) . b
  More g <*> More b = More $ liftA2 ap g b
  More g <*> b      = More $ flip ap b . g

instance Monad (Partial t) where
  return = Done
  Done a >>= k = k a
  More f >>= k = More $ f >=> k

class Feedable t m | m -> t where
  feed :: t -> m a -> m a
  done :: m a -> Bool

instance Feedable t (Partial t) where
  feed t (More f) = f t
  feed _ (Done a) = Done a

  done (Done _) = True
  done (More _) = False

--------------------------------------------------------------------------------

data Position
  = P { offset :: Int
      , column :: Int
      , line   :: Int }
  deriving (Eq)

instance Show Position where
  show p = foldr (++) ""
    [ "@[", show (offset p)
    , ",C", show (column p)
    , ",L", show (line p)
    , "]" ]

type Input t
  = t

data Answer t a
  = Success t Position a        -- unconsumed text, position, parsed value
  | Failure t Position String   -- unconsumed text, position, error message
  deriving (Show, Eq)

data Result t a
  = Partial t (Result t a)

newtype Parser t a
  = Parser (Partial t (Answer t a))


