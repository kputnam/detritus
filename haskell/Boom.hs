import Control.Monad
import Control.Applicative

main :: IO ()
main = return g >> return ()
  where
    f = join (liftA2 (,))
    g = (f.f.f.f.f.f.f.f.f.f.f.f.f.f.f.f.f.f.f.f) id id
