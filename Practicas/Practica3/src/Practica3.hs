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
             (Lam x e) -> "λ" ++ x ++ "." ++ (show e)
             (App x y) -> "(" ++ (show x) ++ " " ++ (show y) ++ ")"


{-
instance Show Expr where
  show e = case e of
             (Var x) -> x
             (Lam x e) -> "\\" ++ x ++ " -> " ++ (show e)
             (App x y) -> "(" ++ (show x) ++ " " ++ (show y) ++ ")"

-}             

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
alphaExpr (Var x) = Var (incrVar x)
--alphaExpr (Lam x e) = if x elem (frVars e) then (Lam (incrVar x) (alphaExpr e)) else (Lam x e)
alphaExpr (Lam x e) =
  let
    nw = findId x (Lam x e)
  in
    otroSubs (Lam x e) (x,nw)
alphaExpr (App e1 e2) = App (alphaExpr e1) (alphaExpr e2)

type OtroSubs = (Identifier, Identifier)

otroSubs :: Expr -> OtroSubs -> Expr
otroSubs (Var x) (x1, x2) = if(x == x1) then (Var x2) else (Var x)
otroSubs (Lam x e) (x1,x2) = if(x == x1) then Lam x2 (otroSubs e (x1,x2)) else Lam x (otroSubs e (x1,x2))
otroSubs (App e1 e2) (x1, x2) = App (otroSubs e1 (x1,x2)) (otroSubs e2 (x1,x2))

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


subst :: Expr -> Substitution -> Expr
subst (Var v) (i,s)
                | v == i = s
                | otherwise = (Var v)
subst (Lam x e) (i,s) 
                | x == i = (Lam x e)
                | x `elem` (frVars s) = Lam (incrVar x) (subst e (i,s))
                | otherwise = Lam x (subst e (i,s))
subst (App e e1) (i,s) = App (subst e (i,s)) (subst e1 (i,s))


--Función que busque una variable y le aplique una función que lleve expresiones a otras expresiones.
magic :: (Expr -> Expr) -> Expr -> Expr
magic f (Var x)         = f (Var x)
magic f (Lam x e) = Lam x (f e)
magic f (App e e1)      = App (f e) (f e1)


--Luego, para reducir en beta, se pasa esa función a una función que toma una expresión y da la expresión de argumento si la expresión es igual al parámetro para
--la función.
betaReduce :: Expr -> Expr
betaReduce (App (Lam var expr1) expr2) = magic (\expr -> if expr == Var var then expr2 else expr) (betaReduce expr1)
betaReduce (App expr1 expr2)                           = let beta1 = betaReduce expr1; beta2 = betaReduce expr2 in
    if beta1 == expr1 then App beta1 beta2 else betaReduce (App beta1 beta2)
betaReduce (Lam (var) expr)                = Lam (var) (betaReduce expr)
betaReduce expr                              = expr


beta :: Expr -> Expr
beta (Var x) = Var x
beta (Lam x e) = Lam x (beta e)
beta (App (Lam x t) y) = (subst (t) (x, y))
beta (App e1 e2) = App e1 (beta e2)


locked :: Expr -> Bool
locked (Var x) = if (beta (Var x) ) == (Var x) then True else False
locked (Lam x e) = if ( beta (Lam x e) ) == (Lam x e) then True else False
locked (App (Lam x t) y) = if (beta (App (Lam x t) y) ) == (App (Lam x t) y)  then True else False
locked (App e1 e2) = if (beta (App e1 e2) ) == (App e1 e2) then True else False


eval :: Expr -> Expr
eval (App e1 e2) = beta (App e1 (beta e2))
eval (Var x) = (Var x)
eval (Lam x e) = beta (Lam x (eval e))
eval (App (Lam x t) y) = (subst (t) (x, y))





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
-----------LOCKED--------
ejemplo12 = locked (Lam "s" (Lam "z" ( Var "z" ) ) )
ejemplo13 = locked (Lam "x" (App (Lam "x" ( Var "x" ))( Var "z" )))

ejemplo14 = eval (App (Lam "n" (Lam "s" (Lam "z" (App ( Var "s" ) (App (App ( Var "n" ) ( Var "s" ) ) ( Var "z" ) ) ) ) ) ) (Lam "s" (Lam "z" ( Var "z" ) ) ) )

cero = Lam "s" (Lam "z" (Var "z"))
uno = Lam "s1" (Lam "z1" (App (Var "s1") (Var "z1")))
suc = Lam "n" (Lam "s2" (Lam "z2" (App (Var "s2") (App (App (Var "n") (Var "s2")) (Var "z2")))))
ejemplo16= betaReduce (App suc cero)
