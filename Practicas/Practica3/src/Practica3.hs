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

import Data.List (union, span, (\\), intersect)

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
    otroSubs (Lam x e) (x,nw)
alphaExpr (App e1 e2) = App (alphaExpr e1) (alphaExpr e2)
--alphaExpr (Lam x e) = if x `elem` (frVars e) then (Lam (incrVar x) (alphaExpr e)) else (Lam x e)


-- | Tipo auxiliar para sustitución de identificadores.
type OtroSubs = (Identifier, Identifier)

-- | otroSubs. Función que dada una expresión y un tipo 'OtroSubs' devuelve
-- |           la expresión equivalente a el segundo elemento de 'OtroSubs'.
otroSubs :: Expr -> OtroSubs -> Expr
otroSubs (Var x) (x1, x2) = if(x == x1) then (Var x2) else (Var x)
otroSubs (Lam x e) (x1,x2) = if(x == x1) then Lam x2 (otroSubs e (x1,x2)) else Lam x (otroSubs e (x1,x2))
otroSubs (App e1 e2) (x1, x2) = App (otroSubs e1 (x1,x2)) (otroSubs e2 (x1,x2))

-- | findId. Función que devuelve un 'Identifier' que no se encuentre en las
-- |         variables de la expresión dada.
findId :: Identifier -> Expr -> Identifier
findId x e =
  let
    x1 = incrVar x
  in
    if x1 `elem` lkVars (Lam x e) || x1 `elem` frVars (Lam x e)
    then findId x1 (Lam x e)
    else x1

-- | subst. Aplica la sustitución a la expresión dada.
subst :: Expr -> Substitution -> Expr
subst e (x,s)  = sub e ((frVars s) `union` (allVars e)) where
  sub (Var v) _
    | v == x = s
    | otherwise = (Var v)
  sub (Lam v e') vs
    | v == x = (Lam v e')
    | v `elem` (frVars s) = Lam (v ++ (newId vs)) (sub e' ((v ++ (newId vs)):vs) )
    | otherwise = Lam v (sub e' vs) where
  sub (App f a) vs = App (sub f vs) (sub a vs)


-- | beta. Aplica un paso de la beta reduccion.
beta :: Expr -> Expr
beta (App (Lam x t) y) = (subst (t) (x, y))
beta (App (Lam x t) e) = App (Lam x t) (beta e)
beta (App e1 e2) = App (beta e1) e2
beta (Lam x t) = Lam x (beta t)
beta (Var x) = Var x
{-
beta (Var x) = Var x
beta (Lam x e) = Lam x (beta e)
beta (App (Lam x t) y) = (subst (t) (x, y))
beta (App e1 e2) = App e1 (beta e2)

-}-- | locked. Determina si una expresion está bloqueada, es decir, no se pueden
-- |         hacer más beta reducciones.
locked :: Expr -> Bool
locked (Var _) = True
locked (Lam x e) = not (x `elem` lkVars e) && locked e
--locked (Lam x e) = locked e
--locked (App e1 e2) = locked e1 && locked e2
locked (App e1 e2) = (lkVars e1) `intersect` (lkVars e2) /=[]

-- | eval. Evalúa una expresión lambda aplicando beta reducciones hasta quedar
-- |       bloqueada.
eval :: Expr -> Expr
--eval (Var x) = (Var x)
eval (Var x) = if(locked (Var x)) then Var x else eval(beta(Var x))
--eval (Lam x e) = eval(beta (Lam x e))
eval (Lam x e) = if(locked (Lam x e)) then Lam x e else eval(beta(Lam x e))
--eval (App e1 e2) = eval (beta (App e1 e2))
eval (App e1 e2) = if(locked (App e1 e2)) then App e1 e2 else eval(beta(App e1 e2))
{-
eval (Lam x e) =
  let
    y = beta(Lam x e)
  in
    if(locked y)
    then y
    else eval y
eval (App e1 e2) =
  let
    y = beta(App e1 e2)
  in
    if(locked y)
    then y
    else eval y
-}
allVars :: Expr -> [Identifier]
allVars (Var x) = [x]
allVars (App e1 e2) = allVars e1 `union` allVars e2
allVars (Lam x e) = allVars e

newId :: [Identifier] -> Identifier
newId vs = head ([ (show i) | i <- [1..]] \\ vs)


{-
allVars :: Expr -> [Identifier]
allVars (Var x) = [x]
allVars (App e1 e2) = allVars e1 `union` allVars e2
allVars (Lam x e) = allVars e

subst ::  Expr -> Substitution -> Expr
sub st e (x,s)  = sub e ((frVars s) `union` (allVars e)) where
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
