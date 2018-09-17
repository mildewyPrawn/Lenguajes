{-
- Lenguajes de Programación 2019-1
- Implementación de Postfix
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
- Kevin Ricardo Miranda Sanchez 314011163 kevinmiranda29@ciencias.unam.mx
-}

module Practica2 where

-- Importamos el modulo que contiene la sintaxis del lenguaje.
import EjerSem02

-- | eval1. Función que devuelve la transición tal que eval1 e = e' syss
-- |        e -> e'.

eval1 :: Exp -> Exp
eval1 (V x) = V x
eval1 (I n) = I n
eval1 (B b) = B b
eval1 (Add a b) = if (isNat a && isNat b)
                  then I(tomaNat a + tomaNat b)
                  else if (isNat a)
                       then (Add a (eval1 b))
                       else (Add (eval1 a) b)
eval1 (Mul a b) = if (isNat a && isNat b)
                  then I(tomaNat a * tomaNat b)
                  else if (isNat a)
                       then (Add a (eval1 b))
                       else (Add (eval1 a) b)
eval1 (Succ a) = if(isNat a)
                 then I(tomaNat a + 1)
                 else Succ(eval1 a)
eval1 (Pred a) = if(isNat a)
                 then I(tomaNat a - 1)
                 else Succ(eval1 a)
eval1 (Not x) = if(esBool x)
                then B (not (tomaBool x))
                else (Not (eval1 x))
eval1 (And a b) = if(esBool a && esBool b)
                  then B(tomaBool a && tomaBool b)
                  else if (esBool a)
                      then And a (eval1 b)
                      else And (eval1 a) b
eval1 (Or a b) = if(esBool a && esBool b)
                 then B(tomaBool a || tomaBool b)
                 else if (esBool a)
                      then And a (eval1 b)
                      else And (eval1 a) b
eval1 (Lt a b) = if(isNat a && isNat b)
                 then B(tomaNat b < tomaNat a)
                 else if(isNat a)
                      then Lt a (eval1 b)
                      else Lt (eval1 a) b
eval1 (Gt a b) = if(isNat a && isNat b)
                 then B(tomaNat a < tomaNat b)
                 else if(isNat a)
                      then Gt a (eval1 b)
                      else Gt (eval1 a) b
eval1 (Eq a b) = if(isNat a && isNat b)
                 then B(tomaNat b == tomaNat a)
                 else if(isNat a)
                      then Eq a (eval1 b)
                      else Eq (eval1 a) b
eval1 (If b t f) = if(esBool b)
                   then if(tomaBool b && True)
                        then t
                        else f
                   else If (eval1 b) t f
eval1 (Let x a b) = if(block a)
                    then subst b (x, a)
                    else  (Let x (eval1 a) b)

-- | evals. Funcion que devuelve la transicion tal que evals e = e' syss
-- |        e ->* e' y e' esta bloqueado.
evals :: Exp -> Exp
evals (V x) = V x
evals (I n) = I n
evals (B b) = B b
evals (Add (I n) (I m)) = I (n + m)
evals (Add (I n) e2) = if(block e2)
                       then (Add (I n) e2)
                       else evals(Add (I n) (evals(e2)))
evals (Add e1 e2) = if (block e1)
                    then (Add e1 e2)
                    else evals(Add (evals e1) e2)
evals (Mul (I n)(I m)) = I (n * m)
evals (Mul (I n) e2) = if(block e2)
                       then (Mul (I n) e2)
                       else evals(Mul (I n) (evals(e2)))
evals (Mul e1 e2) = if(block e1)
                    then (Mul e1 e2)
                    else evals(Mul (evals e1) e2)
evals (Succ (I n)) = I (n + 1)
evals (Succ e1) = if(block e1)
                  then (Succ e1)
                  else evals(Succ (evals e1))
evals (Pred (I n)) = I (n - 1)
evals (Pred e1) = if(block e1)
                  then (Pred e1)
                  else evals(Pred (evals e1))
evals (Not (B b)) = B (not b)
evals (Not x) = if(block x)
                then (Not x)
                else evals(Not (evals x))
evals (And (B p) (B q)) = B(p && q)
evals (And (B p) e2) = if(block e2)
                       then (And (B p) e2)
                       else evals(And (B p) (evals(e2)))
evals (And e1 e2) = if(block e1)
                    then (And e1 e2)
                    else evals(And (evals(e1)) e2)
evals (Or (B p) (B q)) = B(p || q)
evals (Or (B p) e2) = if(block e2)
                      then (Or (B p) e2)
                      else evals(Or (B p) (evals(e2)))
evals (Or e1 e2) = if(block e1)
                   then (Or e1 e2)
                   else evals(Or (evals e1) e2)
evals (Lt (I a) (I b)) = B(b < a)
evals (Lt (I a) e2) = if(block e2)
                      then (Lt (I a) e2)
                      else evals(Lt (I a) (evals(e2)))
evals (Lt e1 e2) = if(block e1)
                   then (Lt e1 e2)
                   else evals(Lt (evals(e1)) e2)
evals (Gt (I a) (I b)) = B(a < b)
evals (Gt (I a) e2) = if(block e2)
                      then (Gt (I a) e2)
                      else evals(Gt (I a) (evals(e2)))
evals (Gt e1 e2) = if(block e1)
                   then (Gt e1 e2)
                   else evals(Gt (evals(e1)) e2)
evals (Eq (I a) (I b)) = B(a == b)
evals (Eq (I a) e2) = if(block e2)
                      then (Eq (I a) e2)
                      else evals(Eq (I a) (evals(e2)))
evals (Eq e1 e2) = if(block e1)
                   then (Eq e1 e2)
                   else evals(Eq (evals e1) e2)
evals (If (B True) e1 e2) = e1
evals (If (B False) e1 e2) = e2
evals (If b e1 e2) = if(block b)
                     then (If b e1 e2)
                     else evals(If (evals b) e1 e2)
evals (Let x (I n) c) = subst c (x, (I n))
evals (Let x (B b) c) = subst c (x, (B b))
evals (Let x a c) = if(block a)
                    then subst c (x, a)
                    else evals(Let x (evals a) c)

-- | eval. Funcion que devuelve la evaluación de un programa tal que eval e = e'
-- |       syss e ->* e' y e' es un valor. En caso de que e' no sea un valor
-- |       deberá mostrar un mensaje de error particular del operador que lo
-- |       causó.


block :: Exp -> Bool
block (V _) = True
block (I _) = True
block (B _) = True
block (Add (I _) (I _)) = False
block (Add a b) = if(isNat (evals a) && isNat(evals b))
                  then False
                  else True
block (Mul (I _) (I _)) = False
block (Mul a b) = if(isNat(evals a) && isNat(evals b))
                  then False
                  else True
block (Succ (I _)) = False
block (Succ a) = if(isNat(evals a))
                 then False
                 else True
block (Pred (I _)) = False
block (Pred a) = if(isNat(evals a))
                 then False
                 else True
block (Not (B _)) = False
block (Not x) = if(esBool(evals x))
                then False
                else True
block (And (B _) (B _)) = False
block (And a b) = if(esBool(evals a) && esBool(evals b))
                  then False
                  else True
block (Or (B _) (B _)) = False
block (Or a b) = if(esBool(evals a) && esBool(evals b))
                 then False
                 else True
block (Lt (I _) (I _)) = False
block (Lt a b) = if(isNat(evals a) && isNat(evals b))
                 then False
                 else True
block (Gt (I _) (I _)) = False
block (Gt a b) = if(isNat(evals a) && isNat(evals b))
                 then False
                 else True
block (Eq (I _) (I _)) = False
block (Eq a b) = if(isNat(evals a) && isNat(evals b))
                 then False
                 else True
block (If (B _) _ _) = False
block (If b _ _) = if(esBool(evals b))
                     then False
                     else True
block (Let _ (I _) _) = False
block (Let _ (B _) _) = False
block (Let _ a _) = if(isNat(evals a) || esBool(evals a)) --Mmmm no lo sé Rick
                    then False
                    else True


tomaBool :: Exp -> Bool
tomaBool (B b) = b
tomaBool _ = error "No es un booleano"

esBool :: Exp -> Bool
esBool (B _) = True
esBool _ = False

isNat :: Exp -> Bool
isNat (I _) = True
isNat _ = False

tomaNat :: Exp -> Int
tomaNat (I n) = n
tomaNat _ = error "no es un número"

