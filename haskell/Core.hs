module Core.Syntax
  ( Expr
  , Definition
  , Program
  , prelude
  ) where

import Data.Text

type Tag          = Int
type Arity        = Int
type Binding      = (Name, Expr)
type Scrutinee    = Expr
type Alternative  = (Int, [Name], Expr)
type Name         = Text
type Bound        = [Name]

-- Expressions parameterized by the type of its binders
data Expr
  = Var Name
  | Num Int
  | Constructor Tag Arity
  | App Expr Expr
  | Let [Binding] Expr
  | Rec [Binding] Expr
  | Case Scrutinee [Alternative]
  | Abs [Name] Expr

type Definition = (Name, [Name], Expr)
type Program    = [Definition]

-- main = double 21
-- double x = x + x
--
-- [("main",   [],    (App (Var "double") (Num 21))
-- ,("double", ["x"], (App (App (Var "+") (Var "x")) (Var "x")))]

prelude :: Program
prelude =
  [("I",        ["x"],          Var "x")
  ,("K",        ["x","y"],      Var "x")
  ,("S",        ["f","g","x"],  (App (App (Var "f") (Var "x"))
                                     (App (Var "g") (Var "x"))))
  ,("flip",     ["f","x","y"],  (App (App (Var "f") (Var "y")) (Var "x")))
  ,("compose",  ["f","g","x"],  (App (Var "f") (App (Var "g") (Var "x"))))
  ,("twice",    ["f"],          (App (App (Var "compose") (Var "f")) (Var "f")))
  ]

