{-# LANGUAGE GADTs #-}

-- Left Identity
--   return x >>= f  == f x
--
-- Right identity
--   m >>= return    == m
--
-- Associativity
--
--   (m >>= f) >>= g == m >>= (\x -> f x >>= g)

data Interaction :: * -> * where
  Say     :: String -> Interaction ()
  Ask     :: Interaction String
  Return  :: a -> Interaction a
  Bind    :: Interaction a -> (a -> Interaction b) -> Interaction b

instance Monad Interaction where
  return = Return
  (>>=)  = Bind

run :: Interaction a -> IO a
run (Say x)    = putStrLn x
run Ask        = getLine
run (Return x) = return x
run (Bind m f) = do x <- run m; run (f x)

say :: String -> Interaction ()
say = Say

ask :: Interaction String
ask = Ask

-- How can we guarantee monad laws by construction?
say' :: String -> (() -> Interaction b) -> Interaction b
say' x = Bind (Say x)

ask' :: (String -> Interaction b) -> Interaction b
ask' = Bind Ask

data Interaction' :: * -> * where
  Say'    :: String -> (() -> Interaction b) -> Interaction b
  Ask'    :: (String -> Interaction b) -> Interaction b
  Return' :: a -> Interaction a

instance Monad Interaction' where
  return         = Return
  Return x >>= f = f x
  Say' m k >>= f = Say' m ((>>= f) . k)
  Ask' k   >>= f = Ask' ((>>= f) . k)

-- Note, Interaction' no longer needs to be a GADT
data Interaction'' a
  = Say'' (() -> Interaction a)
  | Ask'' (String -> Interaction a)
  | Return'' a

