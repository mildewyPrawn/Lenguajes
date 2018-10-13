{-|
Module      : Syntax.MinHs
Description : Provides the syntax of Mini Haskell.
Maintainer  : pablog@ciencias.unam.mx
-}
module Syntax.MinHs (Expr (..)) where

import Syntax.Syntax
import Syntax.Type

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
  | Let Identifier Type Expr Expr
  -- | Function
  | Fun Identifier Type Expr
  -- | Named function
  | FunF Name Identifier Type Type Expr
  -- | Application
  | App Expr Expr
  deriving Eq

instance Show Expr where
  -- |The 'show' function returns a String representation of the MinHs expressions.
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
    Let x t e' e -> "let(" ++ show e' ++ ", (" ++ x ++ " : " ++ show t ++ ")." ++ show e ++ ")"
    Fun x t e -> "f((" ++ x ++ " : " ++ show t ++ ")." ++ show e ++ ")"
    FunF f x t s e -> "f((" ++ x ++ " : " ++ show t ++ ") : " ++ show s++ "." ++ show e ++ ")"
    App e1 e2 -> "(" ++ show e1 ++ " $ " ++ show e2 ++ ")"