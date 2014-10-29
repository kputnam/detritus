
newtype Yoneda f a
  = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

newtype CoYoneda f b
  = CoYoneda { runCoYoneda :: f a -> forall a. (a -> b) }

-- data Coyoneda f b where
--   Coyoneda :: f a -> (a -> b) -> Coyoneda f b

