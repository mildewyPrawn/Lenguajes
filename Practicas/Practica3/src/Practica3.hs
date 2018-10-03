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

import Data.List (union, span, (\\),intersect)

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
             --(Lam x e) -> "λ" ++ x ++ "." ++ (show e)
             (App x y) -> "(" ++ (show x) ++ " " ++ (show y) ++ ")"

-- | Substitution. Tipo que representa la sustitucion.
type Substitution = ( Identifier , Expr )

-- | Reducto. Tipo que representa un reducto del calculo lambda.
type Reducto = (Identifier,Expr)

-- | frVars. Función recursiva que obtiene el conjunto de varialbes libres
-- |         en una expresión.
frVars :: Expr -> [Identifier]
frVars (Var x) = [x]
frVars (Lam x e) = [y | y <- (frVars e) , y/=x ]
frVars (App e1 e2) = [x | x <- (frVars e1 `union` frVars e2)]
--frVars (App e1 e2) = [x | x <- (frVars e1 ++ frVars e2)]

-- | lkVars. Función recursiva que obtiene el conjunto de variables ligadas
-- |         en una expresión.
lkVars :: Expr -> [Identifier]
lkVars (Var _) = []
lkVars (Lam x e) = [x] `union` lkVars e--[y | y <- (lkVars e), y == x]
--lkVars (App e1 e2) = [x | x <- (lkVars e1) `union` (lkVars e2)]
lkVars (App e1 e2) = lkVars e1 `union` lkVars e2

-- | incrVar. Dado un identificador, si este no termina en número, le agrega
-- |          el sufijo '1', en caso contrario toma el valor del número y lo
-- |          incrementa en 1.
incrVar :: Identifier -> Identifier
incrVar xs = if (elem (last xs) (['a'..'z']++['A'..'Z']))
             then xs ++ show 1
             else init xs ++ show((read[last xs])+1)

-- | alphaExpr. Toma una expresión lambda y devuelve una α-equivalente utilizando
-- |            la función 'incrVar' hasta encontrar un nombre que no aparezca en
-- |            cuerpo.
alphaExpr :: Expr -> Expr
alphaExpr (Var x) = Var (incrVar x)
alphaExpr (Lam x e) =
  let
    nw = findId x (Lam x e)
  in
    alphaAux (Lam x e) x nw
alphaExpr (App e1 e2) = App (alphaExpr e1) (alphaExpr e2)

-- | subst. Aplica la sustitución a la expresión dada.
subst :: Expr -> Substitution -> Expr
subst (Var v) (i,s)
                | v == i = s
                | otherwise = (Var v)
subst (Lam x e) (i,s)
                | x == i = (Lam x e)
                | x `elem` (frVars s) = Lam (incrVar x) (subst e (i,s))
                | otherwise = Lam x (subst e (i,s))
subst (App e e1) (i,s) = App (subst e (i,s)) (subst e1 (i,s))

-- | beta. Aplica un paso de la beta reduccion.
beta :: Expr -> Expr
beta (App (Lam x t) y) = (subst (t) (x, y))
--beta (App (Lam x t) e) = App (Lam x t) (beta e)
beta (Lam x e) = Lam x (beta e)
beta (App e1 e2) = App e1 (beta e2)
--beta (App e1 e2) = App (beta e1) e2
beta (Var x) = Var x
--beta e = e

-- | locked. Determina si una expresion está bloqueada, es decir, no se pueden
-- |         hacer más beta reducciones.
locked :: Expr -> Bool
{-locked (Var _) = True
--locked (Lam x e) = locked e
locked a@(Lam x e) = [x] `intersect` (frVars a `union` lkVars a) /= []
                     && locked e
locked (App e1 e2) = (lkVars e1) `intersect` (frVars e2 `union` lkVars e2) /= []
                     && locked e1 && locked e2-}
locked exp = beta(exp) == exp

-- | eval. Evalúa una expresión lambda aplicando beta reducciones hasta quedar
-- |       bloqueada.
eval :: Expr -> Expr
eval x = if x == (evalaux x) then x else beta (evalaux x)
---------------------------------------------------------------------------------------
----                                 FUNCIONES AUXILIARES                           ---
---------------------------------------------------------------------------------------

-- | alphaAux. Función que dada una expresión y un tipo 'OtroSubs' devuelve
-- |           la expresión equivalente a el segundo elemento de 'OtroSubs'.
alphaAux :: Expr -> Identifier -> Identifier -> Expr
alphaAux (Var x) x1 x2 = if (x == x1) then Var x2 else Var x
alphaAux (Lam x e) x1 x2 = if x == x1 then Lam x2 (alphaAux e x1 x2) else Lam x (alphaAux e x1 x2)
alphaAux (App e1 e2) x1 x2 = App (alphaAux e1 x1 x2) (alphaAux e2 x1 x2)

-- | findId. Función que devuelve un 'Identifier' que no se encuentre en las
-- |         variables de la expresión dada.
findId :: Identifier -> Expr -> Identifier
findId x e =
  let
    x1 = incrVar x
  in
    if x1 `elem` lkVars (Lam x e) || x1 `elem` frVars (Lam x e)
    --if elem x1 (lkVars (Lam x e))
    then findId x1 (Lam x e)
    else x1

-- | evalaux. Función recursiva auxiliar que evalúa una función del cálculo lambda.
evalaux :: Expr -> Expr
evalaux (App (Lam v e) d) = eval (subst e (v, d))
evalaux (App e f) = App (eval e) (eval f)
evalaux (Lam x e) = Lam x (eval e)
evalaux e = e

---------------------------------------------------------------------------------------
----                                    PRUEBAS                                     ---
---------------------------------------------------------------------------------------

---------FRVARS----------
ejemplo = frVars (App (Lam "x" (App ( Var "x" ) ( Var "y" ) ) ) (Lam "z" ( Var "z" ) ) )
--Respuesta: ["y"]
ejemplo2 = frVars (Lam "f" (App (App (Var "f")(Lam "x"(App(App(Var "f")(Var "x"))(Var "x" ))))(Lam "x"(App(App(Var "f")(Var "x" ))( Var "x")))))
--Respuesta: []

---------LKVARS----------
ejemplo3 = lkVars (App (Lam "x" (App ( Var "x" ) ( Var "y" ))) (Lam "z" ( Var "z" )))
--Respuesta: ["x","z"]
ejemplo4 = lkVars (Lam "f"(App(App(Var "f" )(Lam "x"(App(App(Var "f")(Var "x" ))(Var "x" ))))(Lam "x" (App(App(Var "f" )(Var "x"))(Var "x" )))))
--Respuesta: ["f","x"]

---------INCRVAR---------
ejemplo5 = incrVar "elem"
--Respuesta: elem1
ejemplo6 = incrVar "x97"
--Respuesta: x98

-------ALPHAEXPR---------
ejemplo7 = alphaExpr (Lam "x" (Lam "y" (App (Var "x" ) (Var "y" ))))
--Respuesta: \x1 -> \y -> (x1 y)
ejemplo8 = alphaExpr (Lam "x" (Lam "x1" (App ( Var "x" ) ( Var "x1"))))
--Respuesta: \x2 -> \x1 -> (x2 x1)

-----------SUBST---------
ejemplo9 = subst (Lam "x" (App ( Var "x" ) ( Var "y" ) ) ) ( "y" , Lam "z" ( Var "z" ))
--Respuesta: \x -> (x \z -> z)
ejemplo10 = subst (Lam "x" ( Var "y" )) ( "y" , Var "x" )
--Respuesta: \x1 -> x

-----BETA-----
ejemplo11 = beta (App (Lam "x" (App ( Var "x" ) ( Var "y" ))) (Lam "z" ( Var "z" )))
--Respuesta: (\z -> z y)
ejemplo20 = beta (App (Lam "n" (Lam "s" (Lam "z" (App ( Var "s" ) (App (App ( Var "n" ) ( Var "s" ) ) ( Var "z" ) ) ) ) ) ) (Lam "s" (Lam "z" ( Var "z" ) ) ) )
--Respuesta: \s -> \z -> (s ((\s -> \z -> z s) z))

-----------LOCKED--------
ejemplo12 = locked (Lam "s" (Lam "z" ( Var "z" ) ) )
--Respuesta: True
ejemplo13 = locked (Lam "x" (App (Lam "x" ( Var "x" ))( Var "z" )))
--Respuesta: False

--------EVAL-------------
ejemplo14 = eval (App (Lam "n" (Lam "s" (Lam "z" (App ( Var "s" ) (App (App ( Var "n" ) ( Var "s" ) ) ( Var "z" ) ) ) ) ) ) (Lam "s" (Lam "z" ( Var "z" ) ) ) )
--Respuesta: \s -> \z -> (s z)
ejemplo15 = eval (App (Lam "n"(Lam "s"(Lam "z" (App (Var "s")(App (App (Var "n")(Var "s"))(Var "z" ))))))(Lam "s" (Lam "z" (App ( Var "s" )(Var "z" )))))
--Respuesta: \s -> \z -> (s (s z))

cero = Lam "s" (Lam "z" (Var "z"))
uno = Lam "s1" (Lam "z1" (App (Var "s1") (Var "z1")))
suc = Lam "n" (Lam "s2" (Lam "z2" (App (Var "s2") (App (App (Var "n") (Var "s2")) (Var "z2")))))
ejemplo16 = eval (App suc cero)
ejemplo17 = eval (App suc uno)
