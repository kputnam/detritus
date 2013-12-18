import Data.Text
import Control.Arrow
import Control.Applicative

data Validation a
  = Includes [(a, Text)]
  | Check (Text -> Maybe a)

instance Functor Validation where
  fmap f (Includes xs)  = Includes (fmap (first f) xs)
  fmap f (Check g)      = Check (\x -> fmap f (g x))

instance Applicative Validation where
  pure                        = Check . const . Just
  Includes fs <*> Includes xs = undefined
  Includes fs <*> Check x     = undefined
  Check f     <*> Check g     = undefined
  Check f     <*> Includes xs = undefined

instance Monad Validation where
