class Term m where
  int  :: Int  -> m Int
  bool :: Bool -> m Bool
  lam  :: (m a -> m b) -> m (a -> b)
  app  :: m (a -> b) -> m a -> m b
  fix  :: (m a -> m a) -> m a
  add  :: m Int -> m Int -> m Int
  mul  :: m Int -> m Int -> m Int
  leq  :: m Int -> m Int -> m Bool
  if_  :: m Bool -> m a -> m a -> m a

newtype Id a = Id { getId :: a }
  deriving (Show, Read, Eq)

instance Term Id where
  int     = Id
  bool    = Id
  lam f   = Id (getId . f . Id)
  app f x = Id $ (getId f) (getId x)
  fix f   = f (fix f)
  add a b = Id (getId a + getId b)
  mul a b = Id (getId a * getId b)
  leq a b = Id (getId a <= getId b)
  if_ c t f = if (getId c) then t else f

idTrue :: Term m => m Bool
idTrue = app (lam (\x -> x)) (bool True)

testPow :: Term m => m (Int -> Int -> Int)
testPow = lam (\x -> fix (\self -> lam (\n ->
                   if_ (leq n (int 0)) (int 1)
                       (mul x (app self (add n (int (-1))))))))
