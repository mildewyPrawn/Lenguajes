{-
- Lenguajes de Programación 2019-1
- Calculo Lambda sin Tipos
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
- Kevin Ricardo Miranda Sanchez 314011163 kevinmiranda29@ciencias.unam.mx
-}

module Practica3 where

import Data.List 

-- | Identifier. Tipo que define un nombre de variables como una cadena de texto.
type Identifier = String

-- | Expr. Tipo que representa una expresion lambda sin tipos.
data Expr = Var Identifier
          | Lam Identifier Expr
          | App Expr Expr deriving(Eq)

-- | Instancia de la clase Show.
instance Show Expr where
  show e = case e of
             (Var x) -> x
             (Lam x e) -> "\\" ++ x ++ " -> " ++ (show e)
             (App x y) -> "(" ++ (show x) ++ " " ++ (show y) ++ ")"

-- | Substitution. Tipo que representa la sustitucion.
type Substitution = ( Identifier , Expr )


frVars :: Expr -> [Identifier]
frVars (Var x) = [x]
frVars (Lam x e) = [y | y <- (frVars e) , y/=x ]
frVars (App e1 e2) = [x | x <- (frVars e1 `union` frVars e2)]
--frVars (App e1 e2) = [x | x <- (frVars e1 ++ frVars e2)]

lkVars :: Expr -> [Identifier]
lkVars (Var _) = []
lkVars (Lam x e) = [x] `union` lkVars e--[y | y <- (lkVars e), y == x]
--lkVars (App e1 e2) = [x | x <- (lkVars e1) `union` (lkVars e2)]
lkVars (App e1 e2) = lkVars e1 `union` lkVars e2

incrVar :: Identifier -> Identifier
incrVar xs = if (elem (last xs) (['a'..'z']++['A'..'Z']))
             then xs ++ show 1
             else init xs ++ show((read[last xs])+1)

alphaExpr :: Expr -> Expr
alphaExpr (App e1 e2) = App (alphaExpr e1) (alphaExpr e2)
alphaExpr (Lam x e) = Lam (incrVar x) (alphaExpr e)
alphaExpr (Var x) = (Var (incrVar x))


subst ::  Expr -> Substitution -> Expr
subst e (i,s)  = sub e ((frVars s) `union` (lkVars e)) where
  sub (Var v) _
    | v == i = s
    | otherwise = (Var v)
  sub (Lam v e) vs
    | v == i = (Lam v e)
    | v `elem` (frVars s) = Lam ((incrVar v)) (sub e (((incrVar v)):vs) )
    | otherwise = Lam v (sub e vs) where
  sub (App e e1) vs = App (sub e vs) (sub e1 vs)



{-
subst :: Expr -> Substitution -> Expr
subst (Var v) (i,s)
                | v == i = s
                | otherwise = (Var v)
subst (Lam x e) (i,s) 
                | x == i = (Lam x e)
                | x `elem` (frVars s) = Lam (incrVar x) (subst e (i,s))
                | otherwise = Lam x (subst e (i,s))
subst (App e e1) (i,s) = App (subst e (i,s)) (subst e1 (i,s))
-}

