module Cont
  ( Cont(..)
  , reset
  , shift
  , ex2
  , ex3
  , ex4
  , ex5
  , ex6
  , run
  ) where

import Control.Applicative

newtype Cont r a = Cont { runCont :: (a -> r) -> r }
  -- Cont    :: ((a -> r) -> r) -> Cont r a
  -- runCont :: Cont r a -> ((a -> r) -> r)

instance Functor (Cont r) where
  -- (a -> b) -> Cont r a -> Cont r b
  fmap f a = Cont $ \k -> runCont a (\x -> k (f x))

instance Applicative (Cont r) where
  -- a -> Cont r a
  pure a  = Cont $ \k -> k a

  -- Cont r (a -> b) -> Cont r a -> Cont r b
  f <*> a = Cont $ \k -> runCont f (\ab -> runCont a (\x -> k (ab x)))

instance Monad (Cont r) where
  -- a -> Cont r a
  return a = Cont $ \k -> k a

  -- Cont r a -> (a -> Cont r b) -> Cont r b
  m >>= f = Cont $ \k -> runCont m (\a -> runCont (f a) k)

-- Execute the continuation within its delimited context
--   reset $ Cont (\k -> k "hello")    ==> "hello"
reset :: Cont r r -> r
reset k = runCont k id

-- 
--  shift (\_ -> Cont (\k -> k "yes")) :: Cont String a
--  shift (\f -> Cont (\_ -> f "yes")) :: Cont r String
shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = Cont $ \k -> reset (f k)

-- Examples
--------------------------------------------------------------------------------

-- Pass the value 2 to the "rest of the computation"
ex2 :: Cont a Int
ex2 = do
  a <- return 1
  b <- Cont $ \k -> k 2
  return $ a + b

-- Disregard the "rest of the computation" and replace it with "escape"
ex3 :: Cont [Char] Int
ex3 = do
  a <- return 1
  b <- Cont $ \_ -> "escape"
  return $ a + b

-- Evaluate the "rest of the computation" with 10 and 20 as parameters,
-- then replace the "rest of the computation" with a different result.
ex4 :: Cont [a] Int
ex4 = do
  a <- return 1
  b <- Cont $ \k -> k 10 ++ k 20
  return $ a + b

-- Here k is (1 + ..)
--   so the result is 1 + (1 + 2) + (1 + 3)
ex5 :: Cont Int Int
ex5 = do
  a <- return 1
  b <- Cont $ \k -> k (k 2 + k 3)
  return $ a + b

i :: Monad m => m a -> Cont (m b) a
i x = Cont $ (x >>=)

run :: Monad m => Cont (m a) a -> m a
run m = runCont m return

ex6 :: Cont [a] Int
ex6 = do
  a <- i [1,2]
  b <- i [10,20]
  return $ a + b
