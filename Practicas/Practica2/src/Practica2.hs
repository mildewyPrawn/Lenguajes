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
-- |        e -> e'

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


block :: Exp -> Bool
block (V x) = True
block (I n) = True
block (B b) = True
block _ = False


tomaBool :: Exp -> Bool
tomaBool (B b) = b
tomaBool t = error "No es un booleano"

esBool :: Exp -> Bool
esBool (B b) = True
esBool t = False

isNat :: Exp -> Bool
isNat (I n) = True
isNat k = False

tomaNat :: Exp -> Int
tomaNat (I n) = n
tomaNat k = error "no es un número"

