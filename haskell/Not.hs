newtype Not a
  = Not { exFalso :: forall r. a -> r }

