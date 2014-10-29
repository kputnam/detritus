module Math.Group.Network
  where

import Math.Group
import Control.Applicative
import qualified Prelude     as P
import qualified Data.Maybe  as P
import qualified Data.Map    as M
import qualified Data.Matrix as X
import qualified Data.Vector as V

-- Multiplexing two inputs: the network links inside the
-- '==' delimiter only transmit one element per unit of
-- time. However we can transmit TWO elements per unit of
-- time to TWO recipents (f and g)
--
--       (m,n)
--      /     \
--     /       \
--  ===============
--  ( b )     ( c )   b(m,n) = m
--    |\       /|     c(m,n) = n
--    | \     / |
--    |  ( d )  |     d(b,c) = b+c
--    |    |    |
--    |  ( e )  |     e(d)   = d
--    | /     \ |
--  ( f )     ( g )   f(b,e) = (b, e + inverse b) = (m,n)
--  ===============   g(e,c) = (e + inverse c, c) = (m,n)
--
network :: Group a -> (a, a) -> ((a, a), (a, a))
network g_ mn = (f (b mn) (e (d (b mn) (c mn)))
                ,g (e (d (b mn) (c mn))) (c mn))
  where
    b = \(m,n) -> m
    c = \(m,n) -> n
    d = \b c   -> (g_ +) b c
    e = \d     -> d
    f = \b e   -> (b, (g_ -) e b)
    g = \e c   -> ((g_ -) e c, c)
