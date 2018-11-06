{-
- Lenguajes de Programación 2019-1
- El lenguaje EAB
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
-}

module EjerSem03 where

import Data.List

--import EjerSem02

type Identifier = String

type Addr = Exp

type Value = Exp

type Cell = (Addr, Value)

type Mem = [Cell]

data Exp = V Identifier | I Int | B Bool | Void | L Int
         | Add Exp Exp | Mul Exp Exp | Succ Exp | Pred Exp
         | Not Exp | And Exp Exp | Or Exp Exp
         | Lt Exp Exp | Gt Exp Exp | Eq Exp Exp
         | If Exp Exp Exp
         | Let Identifier Exp Exp
         | Alloc Exp
         | Deref Exp
         | Assig Exp Exp
         | Seq Exp Exp
         | While Exp Exp deriving (Eq)

-- | Instancia de la clase Show.
instance Show Exp where
  show e = case e of
    (V x) -> "V[" ++ x ++ "]"
    (I n) -> "N[" ++ (show n) ++ "]"
    (B b) -> "B[" ++ (show b) ++ "]"
    (L l) -> "L " ++ (show l)
    (Void) -> "Void"
    (Add a b) -> "Add(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Mul a b) -> "Mul(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Succ x) -> "Succ(" ++ (show x) ++ ")"
    (Pred x) -> "Pred(" ++ (show x) ++ ")"
    (Not x) -> "Not(" ++ (show x) ++ ")"
    (And a b) -> "And(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Or a b) -> "And(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Lt a b) -> "Lt(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Gt a b) -> "Gt(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Eq a b) -> "Eq(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (If p a b) -> "If(" ++ "," ++ (show p) ++ "," ++ (show a) ++ (show b) ++ ")"
    (Let x a b) -> "Let(" ++ (show x) ++ "," ++ (show a) ++ "." ++ (show b)

-- | domain. Dada una memoria, obtiene todas sus direcciones de memoria.
domain :: Mem -> [Int]
domain m = case m of
             [] -> []
             (L l, _):xs -> [l] ++ domain xs
             (_, _):xs -> error "Corrupted memory."

-- | newL. Dada una memoria, genera una nueva dirección de memoria, que no esté
-- |       contenida.
newL :: Mem -> Addr
newL m = case m of
           [] -> L 0
           (L l, o):xs -> if(l+1 `elem` domain m)
                          then newL xs
                          else L (l +
                                  1)

-- | accessM. Dada una dirección de memoria, devuelve el valor contenido en la celda,
-- |          en caso de no encontrarla devuelve Nothing
accessM :: Addr -> Mem -> Maybe Value
accessM _ [] = Nothing
accessM (L l) ((L l', o):xs) = if l == l'
                               then Just o
                               else accessM (L l) xs
accessM (L l) ((_, o):xs) = error "Corrupted memory."

-- | updateM. Dada una celda de memoria, actualiza el valor de esta misma en la memoria.
updateM :: Cell -> Mem -> Mem
updateM _ [] = error "Memory address does not exist."
updateM a@(L l, I n) (b@(L l', I _):xs) = if l == l'
                                          then [(L l, I n)] ++ xs
                                          else [b] ++ updateM a xs
updateM a@(L l, B n) (b@(L l', B _):xs) = if l == l'
                                          then [(L l, B n)] ++ xs
                                          else [b] ++ updateM a xs
updateM (L _, _) ((L l', _):xs) = error "Memory can only store values."
updateM (L _, _) ((_ , _):xs) = error "Corrupted memory."

-- | frVars. Extiende esta funciónpara las nuevas expresiones.
frVars :: Exp -> [Identifier]
frVars (V x) = [x]
frVars (I _) = []
frVars (B _) = []
frVars (Add a b) = frVars a `union` frVars b
frVars (Mul a b) = frVars a `union` frVars b
frVars (Succ x) = frVars x
frVars (Pred x ) = frVars x
frVars (Not x) = frVars x
frVars (And p q) = frVars p `union` frVars q
frVars (Or p q) = frVars p `union` frVars q
frVars (Lt a b) = frVars a `union` frVars b
frVars (Gt a b) = frVars a `union` frVars b
frVars (Eq a b) = frVars a `union` frVars b
frVars (If b p q) = frVars b `union` frVars p `union` frVars q
frVars (Let x p q) = (frVars p `union` frVars q) \\ [x]
frVars (Void) = []
frVars (L _) = []
frVars (Alloc e1) = frVars e1
frVars (Deref e1) = frVars e1
frVars (Assig e1 e2) = frVars e1 `union` frVars e2
frVars (Seq e1 e2) = frVars e1 `union` frVars e2
frVars (While e1 e2) = frVars e1 `union` frVars e2

-- | subst. Extiende esta función para las nuevas expresiones.
subst :: Exp -> Substitution -> Exp
subst (V x) (y, e) = if (x == y) then e else V x
subst (I n) _ = (I n)
subst (B b) _ = (B b)
subst (Add a b) s = Add(subst a s)(subst b s)
subst (Mul a b) s = Mul(subst a s)(subst b s)
subst (Succ x) s = Succ(subst x s)
subst (Pred x) s = Pred(subst x s)
subst (Not x) s = Not(subst x s)
subst (And p q) s = And(subst p s)(subst q s)
subst (Or p q) s = Or(subst p s)(subst q s)
subst (Lt a b) s = Lt(subst a s)(subst b s)
subst (Gt a b) s = Gt(subst a s)(subst b s)
subst (Eq a b) s = Eq(subst a s)(subst b s)
subst (If b p q) s = If(subst b s)(subst p s)(subst q s)
subst (Let x e1 e2) (y,e) = if(elem x ([y] ++  frVars e))
                     then error "Could not apply the substitucion"
                     else Let x (subst e1 (y,e)) (subst e2 (y,e))
subst (Void) = error ""
subst (L _) = error ""
subst (Alloc e1) = error ""
subst (Deref e1) = error ""
subst (Assig e1 e2) = error ""
subst (Seq e1 e2) = error ""
subst (While e1 e2) = error ""

-- | eval1. Extiende esta función para que dada una memoria y una expresión,
-- |        devuelva la reducción a un paso, es decir eval1 (m, e) = (m', e')
-- |        syss <m,e> -> <m',e'>.

-- | evals. Extiende esta función para que dada una memoria y una expresión,
-- |        devolve la expresión hasta que la reducción quede bloqueada, es
-- |        decir evals (m, e) = (m',e') syss <m,e> ->* <m',e'> y e' está
-- |        bloqueda.

-- | eval. Devuelve la evaluación de un programa tal que eval e = e' syss
-- |       <ø,e> ->* <m',e'> y e' es un valor. En caso de que e' no sea un
-- |       valor deberá mostrar un mensaje de error particular al operador qu
-- |       lo causó.

-- | expresión en miniC Que representa un programa con ciclo while.
