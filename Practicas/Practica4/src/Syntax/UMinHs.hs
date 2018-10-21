{-|
Module      : Syntax.UMinHs
Description : Provides the syntax of Untyped Mini Haskell.
Maintainer  : pablog@ciencias.unam.mx
-}
module Syntax.UMinHs (Expr (..)) where

import Syntax.Syntax

data Expr
  -- | Variables, Integer and Boolean constants
  = V Identifier | I Int | B Bool
  -- | Arithmetic operators
  | Add Expr Expr | Mul Expr Expr | Succ Expr | Pred Expr
  -- | Boolean operators
  | And Expr Expr | Or Expr Expr | Not Expr 
  -- | Relational operators
  | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
  -- | If expression
  | If Expr Expr Expr
  -- | Let expression
  | Let Identifier Expr Expr
  -- | Function
  | Fun Identifier Expr
  -- | Named function
  | FunF Name Identifier Expr
  -- | Application
  | App Expr Expr
  deriving Eq

instance Show Expr where
  -- |The 'show' function returns a String representation of the UMinHs expressions.
  show e = case e of
    V x -> "V " ++ show x
    I n -> "I " ++ show n
    B b -> "B " ++ show b
    Add e1 e2 -> "add(" ++ show e1 ++ ", " ++ show e2 ++ ")"
    Mul e1 e2 -> "mul(" ++ show e1 ++ ", " ++ show e2 ++ ")"
    Succ e -> "succ(" ++ show e ++ ")"
    Pred e -> "pred(" ++ show e ++ ")"
    And e1 e2 -> "and(" ++ show e1 ++ ", " ++ show e2 ++ ")"
    Or e1 e2 -> "or(" ++ show e1 ++ ", " ++ show e2 ++ ")"
    Not e -> "not(" ++ show e ++ ")"
    Lt e1 e2 -> "lt(" ++ show e1 ++ ", " ++ show e2 ++ ")"
    Gt e1 e2 -> "gt(" ++ show e1 ++ ", " ++ show e2 ++ ")"
    Eq e1 e2 -> "eq(" ++ show e1 ++ ", " ++ show e2 ++ ")"
    If e1 e2 e3 -> "if(" ++ show e1 ++ ", " ++ show e2 ++ ", " ++ show e3 ++ ")"
    Let x e' e -> "let(" ++ show e' ++ ", " ++ x ++ "." ++ show e ++ ")"
    Fun x e -> "f(" ++ x ++ "." ++ show e ++ ")"
    FunF f x e -> f ++ "(" ++ x ++ "." ++ show e ++ ")"
    App e1 e2 -> "(" ++ show e1 ++ " $ " ++ show e2 ++ ")"
