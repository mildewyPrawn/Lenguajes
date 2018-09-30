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

import Data.List (union, span, (\\))

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

-- | Reducto. Tipo que representa un reducto del calculo lambda.
type Reducto = (Identifier,Expr)

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



{-
allVars :: Expr -> [Identifier]
allVars (Var x) = [x]
allVars (App e1 e2) = allVars e1 `union` allVars e2
allVars (Lam x e) = allVars e

subst ::  Expr -> Substitution -> Expr
subst e (x,s)  = sub e ((frVars s) `union` (allVars e)) where
  sub (Var v) _
    | v == x = s
    | otherwise = (Var v)
  sub (Lam v e') vs
    | v == x = (Lam v e')
    | v `elem` (frVars s) = Lam (v ++ (newId vs)) (sub e' ((v ++ (newId vs)):vs) )
    | otherwise = Lam v (sub e' vs) where
  sub (App f a) vs = App (sub f vs) (sub a vs)


newId :: [Identifier] -> Identifier
newId vs = head ([ (show i) | i <- [1..]] \\ vs)


alphaExpr :: Expr -> Expr
alphaExpr (Var x) = Var x
alphaExpr (Lam x e) = if x `elem` ( e) then (Lam (incrVar x) (alphaExpr e)) else (Lam x e)
alphaExpr (App e1 e2) = App (alphaExpr e1) (alphaExpr e2) 
-}
