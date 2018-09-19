{-
- Lenguajes de Programación 2019-1
- El lenguaje EAB (Semantica)
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
--eval1 (V x) = V x --esta no sé si se pone 3:
eval1 (I _) = error "Ya es una expresion bloqueada."
eval1 (B _) = error "Ya es una expresion bloqueada."
--No las ponemos porque son estados bloqueados, y tiene que morir si no jala.
eval1 (Add a b) = if (isNat a && isNat b)
                  then I(tomaNat a + tomaNat b)
                  else if (isNat a)
                       then (Add a (eval1 b))
                       else (Add (eval1 a) b)
eval1 (Mul a b) = if (isNat a && isNat b)
                  then I(tomaNat a * tomaNat b)
                  else if (isNat a)
                       then (Mul a (eval1 b))
                       else (Mul (eval1 a) b)
eval1 (Succ a) = if(isNat a)
                 then I(tomaNat a + 1)
                 else Succ(eval1 a)
eval1 (Pred a) = if(isNat a)
                 then I(tomaNat a - 1)
                 else Pred(eval1 a)
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
                      then Or a (eval1 b)
                      else Or (eval1 a) b
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
evals (If (B True) e1 _) = e1
evals (If (B False) _ e2) = e2
evals (If b e1 e2) = if(block b)
                     then (If b e1 e2)
                     else evals(If (evals b) e1 e2)
evals (Let x (I n) c) = evals(subst c (x, (I n)))
evals (Let x (B b) c) = evals(subst c (x, (B b)))
evals (Let x a c) = if(block a)
                    then evals(subst c (x, a))
                    else evals(Let x (evals a) c)

-- | eval. Funcion que devuelve la evaluación de un programa tal que eval e = e'
-- |       syss e ->* e' y e' es un valor. En caso de que e' no sea un valor
-- |       deberá mostrar un mensaje de error particular del operador que lo
-- |       causó.
eval :: Exp -> Exp
eval (V x) = V x
eval (I n) = I n
eval (B b) = B b
eval (Add a b) = let
  x = evals(Add a b)
  in
    if(isNat(x))
    then x
    else error "[Add] Expects two Nat."
eval (Mul a b) = let
  x = evals(Mul a b)
  in
    if(isNat x)
    then x
    else error "[Mul] Expects two Nat."
eval (Succ a) = let
  x = evals(Succ a)
  in
    if(isNat x)
    then x
    else error "[Succ] Expects one Nat."
eval (Pred a) = let
  x = evals(Pred a)
  in
    if(isNat x)
    then x
    else error "[Pred] Expects one Nat."
eval (Not x) = let
  x = evals(Not x)
  in
    if(esBool x)
    then x
    else error "[Not] Expects one Boolean."
eval (And a b) = let
  x = evals(And a b)
  in
    if(esBool x)
    then x
    else error "[And] Expects two Boolean."
eval (Or a b) = let
  x = evals(Or a b)
  in
    if(esBool x)
    then x
    else error "[Or] Expects two Boolean."
eval (Lt a b) = let
  x = evals(Lt a b)
  in
    if(esBool x)
    then x
    else error "[Lt] Expects two Nat."
eval (Gt a b) = let
  x = evals(Gt a b)
  in
    if(esBool x)
    then x
    else error "[Gt] Expects two Nat."
eval (Eq a b) = let
  x = evals(Eq a b)
  in
    if(esBool x)
    then x
    else error "[Eq] Expects two Nat."
eval (If b a c) = let
  x = evals(If b a c)
  in
    if x == a || x == b
    then x
    else error "[If] Expects one Boolean and two Exp."
eval (Let y a b) = let
  x = evals(Let y a b)
  in
    if(isNat x || esBool x)
    then x
    else error "[Let] Expects one Var and two Exp."



data Type = Nat | Boolean deriving(Show, Eq)

type Decl = (Identifier, Type)
type TypCtxt = [Decl]

-- | vt. Funcion que verifica el tipado de un programa tal que vt Γ e T = True
-- |     syss Γ ⊢ e:T.
vt :: TypCtxt -> Exp -> Type -> Bool
vt [] (V x) t = False
vt ((a,b):xs) (V x) t = if x == a
                        then if b == t
                             then True
                             else False
                        else vt xs (V x) t
vt l (I n) t = t == Nat
vt l (B b) t = t == Boolean
vt l (Add e1 e2) t = t == Nat &&
                     vt l e1 t &&
                     vt l e2 t
vt l (Mul e1 e2) t = t == Nat &&
                     vt l e1 t &&
                     vt l e2 t
vt l (Succ e) t = vt l e t &&
                  t == Nat
vt l (Pred e) t = vt l e t &&
                  t == Nat
vt l (Not e) t = t == Boolean &&
                 vt l e t
vt l (And e1 e2) t = t == Boolean &&
                     vt l e1 t &&
                     vt l e2 t
vt l (Or e1 e2) t = t == Boolean &&
                     vt l e1 t &&
                     vt l e2 t
vt l (Lt e1 e2) t = t == Boolean &&
                    vt l e1 t &&
                    vt l e2 t
vt l (Gt e1 e2) t = t == Boolean &&
                    vt l e1 t &&
                    vt l e2 t
vt l (Eq e1 e2) t = t == Boolean &&
                    vt l e1 t &&
                    vt l e2 t
vt l (If b e1 e2) t = vt l b Boolean &&
                      vt l e1 t &&
                      vt l e2 t
vt l (Let e1 x e2) t = l++[(e1,t)]--vt l e2 t
--------------------------------------------------------------------------------
--------                       Funciones Auxiliares                     --------
--------------------------------------------------------------------------------
-- | block. Función que nos dice si una expresion está bloqueda o no.
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

-- | tomaBool. Función que devuelve la parte booleana de una EAB.
tomaBool :: Exp -> Bool
tomaBool (B b) = b
tomaBool _ = error "No es un booleano"

-- | esBool. Función que nos dice si una EAB es un booleano.
esBool :: Exp -> Bool
esBool (B _) = True
esBool _ = False

-- | isNat. Función que nos dice si una EAB es un natural.
isNat :: Exp -> Bool
isNat (I _) = True
isNat _ = False

-- | tomaNat. Función que devuelve la parte natural de una EAB.
tomaNat :: Exp -> Int
tomaNat (I n) = n
tomaNat _ = error "no es un número"

--------------------------------------------------------------------------------
--------                             Pruebas                            --------
--------------------------------------------------------------------------------

eval1A = eval1 (Add (I 1) (I 2))
--Resultado: N[3]

eval1B = eval1 (Let "x" (I 1) (Add (V "x") (I 2)))
--Resultado: Add(N[1], N[2])

evals1 = evals (Let "x" (Add (I 1) (I 2)) (Eq (V "x") (I 0)))
--Resultado: B[False]

evals2 = evals (Add (Mul (I 2) (I 6)) (B True))
--Resultado: Add(N[12], B[True])

evalA = eval (Add (Mul (I 2) (I 6)) (B True))
--Resultado: ***Exception: [Add] Expects two Nat.

evalB = eval (Or (Eq (Add (I 0) (I 0)) (I 0)) (Eq (I 1) (I 10)))
--Resultado: B[True]

--vt1 = [("x", Boolean)] (If (B True) (B False) (V "x"))
--Resultado: B[True]

--vt2 = [] (Let "x" (Add (I 1) (I 2))
  --        (Eq (Mul (Add (V "x") (I 5)) (I 0)) (Add (V "x") (I 2))))
--Resultado: B[True]
