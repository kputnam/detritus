{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE RankNTypes     #-}

import Prelude hiding (id, (.))

-- List a
--   = Nil
--   | Cons a (List a)
-- 
--   :: 1 + a*La
-- 
-- Option f
--   = Put f
--   | Call f
--   | Combine (Option f) (Option f)
-- 
--   ::= f + f + O(f)*O(f)
-- 
-- Option f
--   = European (Put | Call) f
--   | Combine (Option f) (Option f)
-- 
--   ::= 2*f + O(f)*O(f)
-- 
-- Contact
--   = Address (Maybe String) (Maybe Int)
-- 
--   ::= (1 + string) * (1 + int)
--     = 1 + string + int + string*int
-- 
-- Contact
--   = Both String Int
--   | Address String
--   | Number Int
-- 
--   ::= (string * int) + string + int
--     = string + int + string*int
-- 
-- |a -> b| = |b| ^ |a|
-- 
-- (x, x) ::= x*x = x^2
-- 2 -> x ::= x^2
-- 
-- --------------------------------------------------------------
-- 
-- a + a = 2 * a
--       = (bool, a)
-- 
-- (a,a) = a*a = a^2
--       = bool -> a
--       = a^2
-- 
--  bool = maybe ()
--     3 = maybe bool
-- 
-- (x, y) ::= x*y
-- 
-- doubling number of discrete int values
--   int + int = 2*int
--             = L int | R int
--             = (bool, int)
--                 ^.
--                   `-- just adding one bit!
-- 
--    0 = _|_
--    1 = ()
--    2 = bool
--    3 = maybe bool (true, false, null)
-- 
--    list a
--      = nil | cons a (list a)
--      = a^0 + a^1 + a^2 + a^3 ...
--        ()          (a,a)
--              (a)         (a,a,a)
--      = 1 + a*L(a)
--      = μL. 1 + a*L
-- 
--    tree a
--      = leaf a | branch (tree a) (tree a)
--      = a + a*a + a*a*a + a*a*a*a + ...
--      = a + T²(a)
--      = μT. a + T²
-- 
-- ∂ for Data
-- 
--   (int, int, int) = int^3
-- 
--   ∂int³ = 3*int²
--     (_, int, int) +   -- int²
--     (int, _, int) +   -- int²
--     (int, int, _)     -- int²
-- 
-- The n-hole context of a type is its nth-derivative
-- 
--   ∂∂int³ = 6*int
--     (_1, _2, int)
--     (_2, _1, int)
--     (_1, int, _2)
--     (_2, int, _1)
--     (int, _1, _2)
--     (int, _2, _1)
-- 
--   ∂∂∂int³ = 6
--     (_1, _2, _3)
--     (_2, _1, _3)
--     (_1, _3, _2)
--     (_2, _3, _1)
--     (_3, _1, _2)
--     (_3, _2, _1)
-- 
--   ∂∂∂∂int³ = 0
--     no values, we can't poke 4 holes in a 3-tuple
-- 
-- Holes have the unit type: _1, _2, _3 :: ().
-- 
-- Kalani Thielen
--   http://blog.lab49.com/archives/3011
-- 
-- Conor McBride Differential Calculus with Datatypes
--   http://strictlypositive.org/calculus/
-- 
-- generatingfunctionology
--   http://www.math.upenn.edu/~wilf/DownldGF.html
-- 
-- Oleg Kiselyov Zipper is the delimited continuation of a traversal
--   function, and the derivative type above is the reification of
--   this delimited control flow as a datatype
--   http://okmij.org/ftp/Computation/Continuations.html#zipper
-- 
-- Chung-chieh Shah on logical interpretation of delimited
--   continuations makes observation that negative types correspond
--   to full-continuations expecting a value of that type
--   http://www.cs.rutgers.edu/~ccshan/polar/paper.pdf
-- 
-- --------------------------------------------------------------
-- 
-- Golden ratio?
-- 
--   T(a) = a + T²(a)
--        = 1 +- sqrt(1-4a)
--         ----------------
--                2
--      φ = 1 + sqrt(5)
--         ------------
--              2
-- 
--        = T(-1)
-- 
-- What is -1?
-- 
--   L(a) = 1 + a*L(a)
--        =    1
--          -------
--           1 - a
-- 
--   L(2) = -1
-- 
-- So a tree labeled by a list of booleans (bitstrings) is our
-- "golden type".
-- 
--     φ² = φ + 1
-- 
-- How should we interpret this identity on the golden ratio?
-- Well, T² is a product (T, T) and T + 1 is the disjoint sum
-- of T and the unit type ().

type Zero
  = forall a. a

data One
  = One

infixl 6 :+:
data a :+: b = L a | R b

infixl 7 :*:
data a :*: b = (:*:) a b

data T a
  = Leaf a
  | Branch (T a) (T a)

type A = T [Bool] :*: T [Bool]
type B = T [Bool] :+: One

-- The identity says these two types are isomorphic.

infixr 0 :~:

data a :~: b
  = Iso { to :: a -> b, from :: b -> a }

class Category c where
  id :: c a a
  (.) :: c x y -> c w x -> c w y

-- Composition of isomorphisms is also an isomorphism
instance Category (:~:) where
  id                  = Iso id id
  Iso f g . Iso f' g' = Iso (f . f') (g . g')

-- T(a) = a + T²(a)
tWrap :: T a :~: (a :+: (T a :*: T a))
tWrap = Iso f g
  where
    f :: T a -> (T a :*: T a) :+: a
    f (Leaf a)      = R a
    f (Branch t u)  = L (t :*: u)

    g :: (T a :*: T a) :+: a -> T a
    g (L (t :*: u)) = Branch t u
    g (R a)         = Leaf a

-- L(a) = 1 + a*L(a)
lWrap :: [a] :~: (One :+: (a :*: [a]))
lWrap = Iso f g
  where
    f :: [a] -> One :+: (a :*: [a])
    f []      = L One
    f (x:xs)  = R (x :*: xs)

    g :: One :+: (a :*: [a]) -> [a]
    g (L One)         = []
    g (R (x :*: xs))  = x:xs


-- Commutativity of addition
commAdd :: (a :+: b) :~: (b :+: a)
commAdd = Iso swap swap
  where
    swap :: a :+: b -> b :+: a
    swap (L a) = R a
    swap (R b) = L b

-- Commutativity of multiplication
commMul :: (a :*: b) :~: (b :*: a)
commMul = Iso swap swap
  where
    swap :: (a :*: b) -> (b :*: a)
    swap (a :*: b) = b :*: a

-- Associativity of addition
assocAdd :: ((a :+: b) :+: c) :~: (a :+: (b :+: c))
assocAdd = Iso f g
  where
    f :: (a :+: b) :+: c -> a :+: (b :+: c)
    f (L (L a)) = L a
    f (L (R b)) = R (L b)
    f (R c)     = R (R c)

    g :: a :+: (b :+: c) -> (a :+: b) :+: c
    g (L a)     = L (L a)
    g (R (L b)) = L (R b)
    g (R (R c)) = R c

-- Associativity of multiplication
assocMul :: ((a :*: b) :*: c) :~: (a :*: (b :*: c))
assocMul = Iso f g
  where
    f :: (a :*: b) :*: c -> a :*: (b :*: c)
    f ((a :*: b) :*: c) = a :*: (b :*: c)

    g :: a :*: (b :*: c) -> (a :*: b) :*: c
    g (a :*: (b :*: c)) = (a :*: b) :*: c

-- Additive identity
-- unitAdd :: (a :+: Zero) :~: a
-- unitAdd = Iso f L
--   where
--     f :: (a :+: Zero) -> a
--     f (L a) = a
--     f (R z) = z

-- Multiplicative identity
unitMul :: (a :*: One) :~: a
unitMul = Iso f g
  where
    f :: (a :*: One) -> a
    f (a :*: _) = a

    g :: a -> (a :*: One)
    g a = a :*: One

-- Distributive law
distMulAdd :: a :*: (b :+: c) :~: (a :*: b) :+: (a :*: c)
distMulAdd = Iso f g
  where
    f :: a :*: (b :+: c) -> (a :*: b) :+: (a :*: c)
    f (a :*: L b) = L (a :*: b)
    f (a :*: R c) = R (a :*: c)

    g :: (a :*: b) :+: (a :*: c) -> a :*: (b :+: c)
    g (L (a :*: b)) = a :*: L b
    g (R (a :*: c)) = a :*: R c

-- Combinators on isomorphisms
-- (+++) :: (a :~: b) -> (c :~: d) -> (a :+: c) :~: (b :+: d)
-- (***) :: (a :~: b) -> (c :~: d) -> (a :*: c) :~: (b :*: d)

------------------------------------------------------------
-- p + q = -p -> q
--     = (p -> _|_) -> q
