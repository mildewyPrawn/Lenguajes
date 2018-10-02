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

           
  {-show e = case e of
             (Var x) -> x
             (Lam x e) -> "λ" ++ x ++ "." ++ (show e)
             (App x y) -> "(" ++ (show x) ++ " " ++ (show y) ++ ")"
-}




-- | Substitution. Tipo que representa la sustitucion.
type Substitution = ( Identifier , Expr )

--frVars. Obtiene el conjunto de variables libres de una expresion.
frVars :: Expr -> [Identifier]
frVars (Var x) = [x]
frVars (Lam x e) = [y | y <- (frVars e) , y/=x ]
frVars (App e1 e2) = [x | x <- (frVars e1 `union` frVars e2)]
--frVars (App e1 e2) = [x | x <- (frVars e1 ++ frVars e2)]


--lkVars. Obtiene el conjunto de variables ligadas de una expresion.
lkVars :: Expr -> [Identifier]
lkVars (Var _) = []
lkVars (Lam x e) = [x] `union` lkVars e--[y | y <- (lkVars e), y == x]
--lkVars (App e1 e2) = [x | x <- (lkVars e1) `union` (lkVars e2)]
lkVars (App e1 e2) = lkVars e1 `union` lkVars e2


--incrVar. Dado un identificador, si este no termina en n ́umero
--le agrega el sufijo 1, en caso contrario toma el valor del n ́umero y lo
--incrementa en 1.
incrVar :: Identifier -> Identifier
incrVar xs = if (elem (last xs) (['a'..'z']++['A'..'Z']))
             then xs ++ show 1
             else init xs ++ show((read[last xs])+1)

--alphaExpr. Toma una expresion lambda y devuelve una α- equivalente 
--utilizando la funcion incrVar hasta encontrar un nombre que no aparezca
-- en el cuerpo.
alphaExpr :: Expr -> Expr
alphaExpr (Var x) = Var (incrVar x)
alphaExpr (Lam x e) =
  let
    nw = findId x (Lam x e)
  in
    alphaAux (Lam x e) x nw
alphaExpr (App e1 e2) = App (alphaExpr e1) (alphaExpr e2)

alphaAux :: Expr -> Identifier -> Identifier -> Expr
alphaAux (Var x) x1 x2 = if(x == x1) then (Var x2) else (Var x)
alphaAux (Lam x e) x1 x2 = if(x == x1) then Lam x2 (alphaAux e x1 x2) else Lam x (alphaAux e x1 x2)
alphaAux (App e1 e2) x1 x2 = App (alphaAux e1 x1 x2) (alphaAux e2 x1 x2 )

findId :: Identifier -> Expr -> Identifier
findId x e =
  let
    x1 = incrVar x
  in
    if elem x1 (lkVars (Lam x e))
    then findId x1 (Lam x e)
    else x1


{-alphaExpr :: Expr -> Expr
alphaExpr (App e1 e2) = App (alphaExpr e1) (alphaExpr e2)
alphaExpr (Lam x e) = Lam (incrVar x) (alphaExpr e)
alphaExpr (Var x) = (Var (incrVar x))
-}

{-
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
-}

-- subst. Aplica la sustituci ́on a la expresi ́on dada.
subst :: Expr -> Substitution -> Expr
subst (Var v) (i,s)
                | v == i = s
                | otherwise = (Var v)
subst (Lam x e) (i,s) 
                | x == i = (Lam x e)
                | x `elem` (frVars s) = Lam (incrVar x) (subst e (i,s))
                | otherwise = Lam x (subst e (i,s))
subst (App e e1) (i,s) = App (subst e (i,s)) (subst e1 (i,s))

-- beta. Aplica un paso de la beta reduccion.
beta :: Expr -> Expr
beta (App (Lam x t) y) = (subst (t) (x, y))
beta (Lam x e) = Lam x (beta e)
beta (App e1 e2) = App e1 (beta e2)
beta e = e

-- locked. Determina si una expresion esta bloqueada, es decir, no se pueden hacer mas beta reducciones.
locked :: Expr -> Bool
locked (Var x) = if (beta (Var x) ) == (Var x) then True else False
locked (Lam x e) = if (beta (Lam x e) ) == (Lam x e) then True else False
locked (App (Lam x t) y) = if locked (beta (App (Lam x t) y) ) then True else False
locked (App e1 e2) = if locked(beta (App e1 e2)) then True else False

{-
locked e = if e == (beta e) || alphaExpr e == alphaExpr(eval e) then True else False
-}



--eval. Evalua una expresion lambda aplicando beta reducciones hasta quedar bloqueada.
evalaux :: Expr -> Expr
evalaux (App (Lam v e) d) = eval (subst e (v, d))
evalaux (App e f) = App (eval e) (eval f)
evalaux (Lam x e) = Lam x (eval e)
evalaux e = e  

eval :: Expr -> Expr
eval x = if x == (evalaux x) then x else beta (evalaux x)


ejemplonofunciona = subst (Lam "x" ( Var "x" )) ( "x" , Var "y" )

-------------------------------------------EJEMPLOS---------------------------------------------------------------

---FRVARS-----------
ejemplo = frVars (App (Lam "x" (App ( Var "x" ) ( Var "y" ) ) ) (Lam "z" ( Var "z" ) ) )
ejemplo2 = frVars (Lam "f" (App (App (Var "f")(Lam "x"(App(App(Var "f")(Var "x"))(Var "x" ))))(Lam "x"(App(App(Var "f")(Var "x" ))( Var "x")))))
---LKVARS---------
ejemplo3 = lkVars (App (Lam "x" (App ( Var "x" ) ( Var "y" ))) (Lam "z" ( Var "z" )))
ejemplo4 = lkVars (Lam "f"(App(App(Var "f" )(Lam "x"(App(App(Var "f")(Var "x" ))(Var "x" ))))(Lam "x" (App(App(Var "f" )(Var "x"))(Var "x" )))))
----INCRVAR----
ejemplo5 = incrVar "elem"
ejemplo6 = incrVar "x97"

-----ALPHAEXPR-------------------------
ejemplo7 = alphaExpr (Lam "x" (Lam "y" (App (Var "x" ) (Var "y" ))))
ejemplo8 = alphaExpr (Lam "x" (Lam "x1" (App ( Var "x" ) ( Var "x1"))))

--------SUBST------
ejemplo9 = subst (Lam "x" (App ( Var "x" ) ( Var "y" ) ) ) ( "y" , Lam "z" ( Var "z" ))
ejemplo10 = subst (Lam "x" ( Var "y" )) ( "y" , Var "x" )

-----BETA-----
ejemplo11 = beta (App (Lam "x" (App ( Var "x" ) ( Var "y" ))) (Lam "z" ( Var "z" )))
ejemplo20 = beta (App (Lam "n" (Lam "s" (Lam "z" (App ( Var "s" ) (App (App ( Var "n" ) ( Var "s" ) ) ( Var "z" ) ) ) ) ) ) (Lam "s" (Lam "z" ( Var "z" ) ) ) )
-----------LOCKED--------
ejemplo12 = locked (Lam "s" (Lam "z" ( Var "z" ) ) )
ejemplo13 = locked (Lam "x" (App (Lam "x" ( Var "x" ))( Var "z" )))

ejemplo14 = eval (App (Lam "n" (Lam "s" (Lam "z" (App ( Var "s" ) (App (App ( Var "n" ) ( Var "s" ) ) ( Var "z" ) ) ) ) ) ) (Lam "s" (Lam "z" ( Var "z" ) ) ) )
ejemplo15 = eval (App (Lam "n"(Lam "s"(Lam "z" (App (Var "s")(App (App (Var "n")(Var "s"))(Var "z" ))))))(Lam "s" (Lam "z" (App ( Var "s" )(Var "z" )))))

cero = Lam "s" (Lam "z" (Var "z"))
uno = Lam "s1" (Lam "z1" (App (Var "s1") (Var "z1")))
suc = Lam "n" (Lam "s2" (Lam "z2" (App (Var "s2") (App (App (Var "n") (Var "s2")) (Var "z2")))))
ejemplo16 = eval (App suc cero)
ejemplo17 = eval (App suc uno)


