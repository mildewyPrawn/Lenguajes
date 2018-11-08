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

type Identifier = String

type Addr = Exp

type Value = Exp

type Cell = (Addr, Value)

type Mem = [Cell]

type Substitution = (Identifier, Exp)

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
                          else L (l + 1)

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
updateM a@(L l, Void) (b@(L l', _):xs) = if l == l'
                                            then [(L l, Void)] ++ xs
                                            else [b] ++ updateM a xs
updateM a@(L l, I n) (b@(L l', _):xs) = if l == l'
                                          then [(L l, I n)] ++ xs
                                          else [b] ++ updateM a xs
updateM a@(L l, B n) (b@(L l', _):xs) = if l == l'
                                          then [(L l, B n)] ++ xs
                                          else [b] ++ updateM a xs
updateM _ [] = error "Memory address does not exist."
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
frVars (Let x p q) = frVars p `union` ([y | y <- frVars q, y /= x])
frVars (Void) = []
frVars (L _) = []
frVars (Alloc e1) = frVars e1
frVars (Deref e1) = frVars e1
frVars (Assig e1 e2) = frVars e1 `union` frVars e2
frVars (Seq e1 e2) = frVars e1 `union` frVars e2
frVars (While e1 e2) = frVars e1 `union` frVars e2

-- | subst. Extiende esta función para las nuevas expresiones.
subst :: Exp -> Substitution -> Exp
subst (V x) (y, e) = if (x == y)
                     then e
                     else V x
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
                            then error "Could not apply the substitution"
                            else Let x (subst e1 (y,e)) (subst e2 (y,e))
subst (Void) _ = Void
subst (L l) _ = (L l)
subst (Alloc e1) s = Alloc(subst e1 s)
subst (Deref e1) s = Deref(subst e1 s)
subst (Assig e1 e2) s = Assig(subst e1 s) (subst e2 s)
subst (Seq e1 e2) s = Seq(subst e1 s) (subst e2 s)
subst (While e1 e2) s = While(subst e1 s) (subst e2 s)

-- | eval1. Extiende esta función para que dada una memoria y una expresión,
-- |        devuelva la reducción a un paso, es decir eval1 (m, e) = (m', e')
-- |        syss <m,e> -> <m',e'>.
eval1 :: (Mem, Exp) -> (Mem, Exp)
--eval1 p = error ""
eval1 (m, e) = case e of
                (V _) -> error "Ya es una expresión bloqueada"
                (I _) -> error "Ya es una expresión bloqueada"
                (B _) -> error "Ya es una expresión bloqueda"
                (Add e1 e2) -> if (isNat e1 && isNat e2)
                               then (m, I(tomaNat e1 + tomaNat e2))
                               else if (isNat e1)
                                    then let (m', e') = eval1 (m, e2)
                                         in
                                           (m', (Add e1 e'))
                                    else let (m', e') = eval1 (m, e1)
                                         in
                                           (m', (Add e' e2))
                (Mul e1 e2) -> if (isNat e1 && isNat e2)
                               then (m, I(tomaNat e1 * tomaNat e2))
                               else if (isNat e1)
                                    then let (m', e') = eval1 (m, e2)
                                         in
                                           (m', (Mul e1 e'))
                                    else let (m', e') = eval1 (m, e1)
                                         in
                                           (m', (Mul e' e2))
                (Succ e1) -> if (isNat e1)
                             then (m, I(tomaNat e1 + 1))
                             else let (m', e') = eval1 (m, e1)
                                  in
                                    (m', Succ e')
                (Pred e1) -> if (isNat e1)
                             then (m, I(tomaNat e1 - 1))
                             else let (m', e') = eval1 (m, e1)
                                  in
                                    (m', Pred e')
                (Not e1) -> if (esBool e1)
                            then (m, B(not (tomaBool e1)))
                            else let (m', e') = eval1 (m, e1)
                                 in
                                   (m', Not e')
                (And e1 e2) -> if (esBool e1 && esBool e2)
                               then (m, B(tomaBool e1 && tomaBool e2))
                               else if (esBool e1)
                                    then let (m', e') = eval1 (m, e2)
                                         in
                                           (m', (And e1 e'))
                                    else let (m', e') = eval1 (m, e1)
                                         in
                                           (m', (And e' e2))
                (Or e1 e2) -> if (esBool e1 && esBool e2)
                               then (m, B(tomaBool e1 || tomaBool e2))
                               else if (esBool e1)
                                    then let (m', e') = eval1 (m, e2)
                                         in
                                           (m', (Or e1 e'))
                                    else let (m', e') = eval1 (m, e1)
                                         in
                                           (m', (Or e' e2))
                (Lt e1 e2) -> if (isNat e1 && isNat e2)
                              then (m, B(tomaNat e2 < tomaNat e1))
                              else if (isNat e1)
                                   then let (m', e') = eval1 (m, e2)
                                        in
                                          (m', (Lt e1 e'))
                                   else let (m', e') = eval1 (m, e1)
                                        in
                                           (m', (Lt e' e2))
                (Gt e1 e2) -> if (isNat e1 && isNat e2)
                              then (m, B(tomaNat e1 < tomaNat e2))
                              else if (isNat e1)
                                   then let (m', e') = eval1 (m, e2)
                                        in
                                          (m', (Gt e1 e'))
                                   else let (m', e') = eval1 (m, e1)
                                        in
                                           (m', (Gt e' e2))
                (Eq e1 e2) -> if (isNat e1 && isNat e2)
                              then (m, B(tomaNat e1 == tomaNat e2))
                              else if (isNat e1)
                                   then let (m', e') = eval1 (m, e2)
                                        in
                                          (m', (Eq e1 e'))
                                   else let (m', e') = eval1 (m, e1)
                                        in
                                           (m', (Eq e' e2))
                (If b e1 e2) -> if(esBool b)
                                then if(tomaBool b && True)
                                     then (m, e1)
                                     else (m, e2)
                                else let (m', b') = eval1 (m, b)
                                     in
                                       (m', If b' e1 e2)
                (Let x e1 e2) -> if(block e1)
                                 then (m, subst e2 (x, e1))
                                 else let (m', e') = eval1 (m, e1)
                                      in
                                        (m', Let x e' e2)
                (Void) -> error "Ya es una expresión bloqueda."
                (L _) -> error "Esto no debería estar aquí."
                (Alloc e1) -> if(block e1)
                              then (((newL m),e1):m, newL m)
                              else let (m', e') = eval1 (m, e1)
                                   in
                                     (m', Alloc e1)
                                     {-
                (Deref d@(L l)) -> case accessM d of
                                     Nothing -> error "Memory does not exist"
                                     Just a -> (m, a)
                (Deref e1) -> let (m',e') = eval1 (m, e1)
                              in
                                (m', Deref e')-}
                (Deref e1) -> if(block e1)
                              then case accessM e1 of
                                     Nothing -> error "Memory does not exist"
                                     Just a -> (m, a)
                              else let (m',e') = eval1 (m, e1)
                                   in
                                     (m', Deref e')
                (Assig e1 e2) -> if(block e1 && block e2)
                                 then (updateM (e1,e2) m, Void)
                                 else let (m', e') = eval1 (m, e1)
                                      in
                                        (m', Assig e' e2)
                (Seq Void e2) -> (m, e2)
                (Seq e1 e2) -> let (m',e') = eval1 (m, e1)
                               in
                                 (m', Seq e' e2)
                w@(While e1 e2) -> eval1(m, If (e1) (Seq e1 w) Void)
--eval1 let x e1 e2 = if value(e1) then subst e2 (x, e1) else let x (eval1 e1) e2

-- | evals. Extiende esta función para que dada una memoria y una expresión,
-- |        devolve la expresión hasta que la reducción quede bloqueada, es
-- |        decir evals (m, e) = (m',e') syss <m,e> ->* <m',e'> y e' está
-- |        bloqueda.

-- | eval. Devuelve la evaluación de un programa tal que eval e = e' syss
-- |       <ø,e> ->* <m',e'> y e' es un valor. En caso de que e' no sea un
-- |       valor deberá mostrar un mensaje de error particular al operador qu
-- |       lo causó.

-- | expresión en miniC Que representa un programa con ciclo while.



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
block (Void) = True
block (L _) = True
block (Alloc _) = False
block (Deref _) = False
block (Assig _ _) = False
block (Seq _ _) = False
block (While _ _) = False

{-locked Void = True
locked (L _) = True
locked (V _) = True
locked (I _) = True
locked (B _) = True
locked (Add (I _) (I _)) = False
--locked add (i n) e2 = locked e2 -- podriamos no ponerla
locked (Add e1 e2) = locked e1 && locked e2
locked (Mul (I _) (I _)) = False
locked (Mul e1 e2) = locked e1 && locked e2
locked (Succ (I _)) = 
locked (Succ e1) = locked
-}
--locked let x e1 e2 = if value e1 then false else locked e1

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

