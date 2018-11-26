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

import Data.Void

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
    (Seq e1 e2) -> "Seq(" ++ (show e1) ++ "; " ++ (show e2) ++ ")"
    (While e1 e2) -> "While(" ++ (show e1) ++ " do " ++ (show e2) ++ ")"
    (Alloc e1) -> "Alloc(" ++ (show e1) ++ ")"
    (Assig e1 e2) -> "Assig(" ++ (show e1) ++ ", " ++ (show e2) ++ ")"
    (Deref e1) -> "Deref(" ++ (show e1) ++ ")"

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
updateM _ [] = error "Memory address does not exist."
updateM a@(L l, Void) (b@(L l', _):xs) = if l == l'
                                         then [(L l, Void)] ++ xs
                                         else [b] ++ updateM a xs
updateM a@(L l, I n) (b@(L l',  _):xs) = if l == l'
                                          then [(L l, I n)] ++ xs
                                          else [b] ++ updateM a xs
updateM a@(L l, B n) (b@(L l',  _):xs) = if l == l'
                                          then [(L l, B n)] ++ xs
                                          else [b] ++ updateM a xs
updateM (L _, _) ((L _, _):xs) = error "Memory can only store values."
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
eval1 (m, e) = case e of
                 (V v) -> error "Ya es una expresión bloqueada"
                 (I n) -> error "Ya es una expresión bloqueada"
                 (B b) -> error "Ya es una expresión bloqueda"
                 (Add e1 e2) -> if (isNat(m, e1) && isNat(m, e1))
                               then (m, I(tomaNat e1 + tomaNat e2))
                               else if (isNat(m, e1))
                                    then let (m', e') = eval1 (m, e2)
                                         in
                                           (m', (Add e1 e'))
                                    else let (m', e') = eval1 (m, e1)
                                         in
                                           (m', (Add e' e2))
                 (Mul e1 e2) -> if (isNat(m, e1) && isNat(m, e1))
                                then (m, I(tomaNat e1 * tomaNat e2))
                                else if (isNat(m, e1))
                                     then let (m', e') = eval1 (m, e2)
                                          in
                                            (m', (Mul e1 e'))
                                     else let (m', e') = eval1 (m, e1)
                                          in
                                            (m', (Mul e' e2))
                 (Succ e1) -> if (isNat(m, e1))
                              then (m, I(tomaNat e1 + 1))
                              else let (m', e') = eval1 (m, e1)
                                   in
                                     (m', Succ e')
                 (Pred e1) -> if (isNat(m, e1))
                             then (m, I(tomaNat e1 - 1))
                             else let (m', e') = eval1 (m, e1)
                                  in
                                    (m', Pred e')
                 (Not e1) -> if (esBool(m, e1))
                             then (m, B(not (tomaBool e1)))
                             else let (m', e') = eval1 (m, e1)
                                  in
                                    (m', Not e')
                 (And e1 e2) -> if (esBool(m, e1) && esBool(m, e2))
                                then (m, B(tomaBool e1 && tomaBool e2))
                                else if (esBool(m, e1))
                                     then let (m', e') = eval1 (m, e2)
                                          in
                                            (m', (And e1 e'))
                                     else let (m', e') = eval1 (m, e1)
                                          in
                                            (m', (And e' e2))
                 (Or e1 e2) -> if (esBool(m, e1) && esBool(m, e2))
                               then (m, B(tomaBool e1 || tomaBool e2))
                               else if (esBool(m, e1))
                   then let (m', e') = eval1 (m, e2)
                        in
                          (m', (Or e1 e'))
                                    else let (m', e') = eval1 (m, e1)
                                         in
                                           (m', (Or e' e2))
                 (Lt e1 e2) -> if (isNat(m, e1) && isNat(m, e1))
                               then (m, B(tomaNat e2 < tomaNat e1))
                               else if (isNat(m, e1))
                                    then let (m', e') = eval1 (m, e2)
                                         in
                                           (m', (Lt e1 e'))
                                    else let (m', e') = eval1 (m, e1)
                                         in
                                           (m', (Lt e' e2))
                 (Gt e1 e2) -> if (isNat(m, e1) && isNat(m, e1))
                               then (m, B(tomaNat e1 < tomaNat e2))
                               else if (isNat(m, e1))
                                    then let (m', e') = eval1 (m, e2)
                                         in
                                           (m', (Gt e1 e'))
                                    else let (m', e') = eval1 (m, e1)
                                         in
                                           (m', (Gt e' e2))
                 (Eq e1 e2) -> if (isNat(m, e1) && isNat(m, e1))
                               then (m, B(tomaNat e1 == tomaNat e2))
                               else if (isNat(m, e1))
                                    then let (m', e') = eval1 (m, e2)
                                         in
                                           (m', (Eq e1 e'))
                                    else let (m', e') = eval1 (m, e1)
                                         in
                                           (m', (Eq e' e2))
                 (If b e1 e2) -> if(esBool (m, b))
                                 then if(tomaBool b && True)
                                      then (m, e1)
                                      else (m, e2)
                                 else let (m', b') = eval1 (m, b)
                                      in
                                        (m', If b' e1 e2)
                 (Let x e1 e2) -> if(block (m, e1))
                                  then (m, subst e2 (x, e1))
                                  else let (m', e') = eval1 (m, e1)
                                       in
                                         (m', Let x e' e2)
                 (Void) -> error "Ya es una expresión bloqueda."
                 (L _) -> error "Esto no debería estar aquí."
                 (Alloc e1) -> if(block (m, e1))
                               then (((newL m),e1):m, newL m)
                               else let (m', e') = eval1 (m, e1)
                                    in
                                      (m', Alloc e1)
                 (Deref l@(L _)) -> case accessM l m of
                                      Nothing -> error "Memory address does not exist"
                                      Just v -> (m,v)
                 (Deref e1) -> let (m',e') = eval1 (m,e1) in (m', Deref e')
                 (Assig e1 e2) -> if(block (m, e1) && block (m, e2))
                                  then (updateM (e1,e2) m, Void)
                                  else let (m', e') = eval1 (m, e1)
                                       in
                                         (m', Assig e' e2)
                 (Seq Void e2) -> (m, e2)
                 (Seq e1 e2) -> let (m',e') = eval1 (m, e1)
                                in
                                  (m', Seq e' e2)
                 w@(While e1 e2) -> (m, If (e1) (Seq e2 w) Void)
  --eval1 let x e1 e2 = if value(e1) then subst e2 (x, e1) else let x (eval1 e1) e2

-- | evals. Extiende esta función para que dada una memoria y una expresión,
-- |        devolve la expresión hasta que la reducción quede bloqueada, es
-- |        decir evals (m, e) = (m',e') syss <m,e> ->* <m',e'> y e' está
-- |        bloqueda.

evals :: (Mem, Exp) -> (Mem, Exp)
evals (m, e) = case e of
                 (V v) -> (m, V v)
                 (I n) -> (m, I n)
                 (B b) -> (m, B b)
                 (Add (I n1) (I n2)) -> (m, I(n1 + n2))
                 (Add (I n1) e2) -> if (block (m,e2))
                                    then (m, Add (I n1) e2)
                                    else let (m', e') = evals (m, e2)
                                         in
                                           evals(m', Add (I n1) e')
                 (Add e1 e2) -> if(block (m, e1))
                                then (m, Add e1 e2)
                                else let (m',e') = evals(m, e1)
                                     in
                                       evals(m', Add e' e2 )
                 (Mul (I n1) (I n2)) -> (m, I(n1 * n2))
                 (Mul (I n1) e2) -> if (block (m,e2))
                                    then (m, Mul (I n1) e2)
                                    else let (m', e') = evals (m, e2)
                                         in
                                           evals(m', Mul (I n1) e')
                 (Mul e1 e2) -> if(block (m, e1))
                                then (m, Mul e1 e2)
                                else let (m',e') = evals(m, e1)
                                     in
                                       evals(m', Mul e' e2 )
                 (Succ (I n)) -> (m, I(n + 1))
                 (Succ e1) -> if(block (m, e1))
                              then (m, Succ e1)
                              else let (m', e') = evals (m, e1)
                                   in
                                     (m', e')
                 (Pred (I n)) -> (m, I(n - 1))
                 (Pred e1) -> if(block (m, e1))
                              then (m, Pred e1)
                              else let (m', e') = evals (m, e1)
                                   in
                                     (m', e')
                 (Not (B b)) -> (m, B(not b))
                 (Not e1) -> if(block (m, e))
                             then (m, Not e1)
                             else let (m', e') = evals(m, e1)
                                  in
                                    evals(m', Not e')
                 (And (B n1) (B n2)) -> (m, B(n1 && n2))
                 (And (B n1) e2) -> if (block (m,e2))
                                    then (m, And (B n1) e2)
                                    else let (m', e') = evals (m, e2)
                                         in
                                           evals(m', And (B n1) e')
                 (And e1 e2) -> if(block (m, e1))
                                then (m, And e1 e2)
                                else let (m',e') = evals(m, e1)
                                     in
                                       evals(m', And e' e2 )
                 (Or (B n1) (B n2)) -> (m, B(n1 || n2))
                 (Or (B n1) e2) -> if (block (m,e2))
                                    then (m, Or (B n1) e2)
                                    else let (m', e') = evals (m, e2)
                                         in
                                           evals(m', Or (B n1) e')
                 (Or e1 e2) -> if(block (m, e1))
                                then (m, Or e1 e2)
                                else let (m',e') = evals(m, e1)
                                     in
                                       evals(m', Or e' e2 )
                 (Lt (I a) (I b)) -> (m, B(b < a))
                 (Lt (I a) e2) -> if(block (m, e2))
                                  then (m, Lt (I a) e2)
                                  else let (m', e') = evals(m, e2)
                                       in
                                         evals(m', Lt (I a) e')
                 (Lt e1 e2) -> if(block (m, e1))
                               then (m, Lt e1 e2)
                               else let (m', e') = evals(m, e1)
                                    in
                                      (m', Lt e' e2)
                 (Gt (I a) (I b)) -> (m, B(a < b))
                 (Gt (I a) e2) -> if(block (m, e2))
                                  then (m, Gt (I a) e2)
                                  else let (m', e') = evals(m, e2)
                                       in
                                         evals(m', Gt (I a) e')
                 (Gt e1 e2) -> if(block (m, e1))
                               then (m, Gt e1 e2)
                               else let (m', e') = evals(m, e1)
                                    in
                                      (m', Gt e' e2)
                 (Eq (I a) (I b)) -> (m, B(a == b))
                 (Eq (I a) e2) -> if(block (m, e2))
                                  then (m, Eq (I a) e2)
                                  else let (m', e') = evals(m, e2)
                                       in
                                         evals(m', Eq (I a) e')
                 (Eq e1 e2) -> if(block (m, e1))
                               then (m, Eq e1 e2)
                               else let (m', e') = evals(m, e1)
                                    in
                                      (m', Gt e' e2)
                 (If (B True) e1 _) -> (m, e1)
                 (If (B False) _ e2) -> (m, e2)
                 i@(If b e1 e2) -> if(block (m, b))
                                   then (m, i)
                                   else let (m', b') = evals (m, b)
                                        in
                                          evals(m', If b' e1 e2)
                 (Let x (I n) c) -> evals(m, subst c (x, I n))
                 (Let x (B b) c) -> evals(m, subst c (x, B b))
                 (Let x a c) -> if(block(m, a))
                                then evals(m, subst c(x,a))
                                else let (m', a') = evals(m, a)
                                     in-- evals(m, Let x (evals(m, a)) c)
                                       evals(m', Let x a' c)
                 (Void) -> (m, Void)
                 (L _) -> error "Esto no debería estar aquí."
                 (Alloc e1) -> if(block (m, e1))
                               then (((newL m),e1):m, newL m)
                               else let (m', e') = evals (m, e1)
                                    in
                                      (m', Alloc e')
                 (Deref l@(L _)) -> case accessM l m of
                                      Nothing -> error "Memory address does not exist"
                                      Just v -> (m,v)
                 (Deref e1) -> let (m',e') = evals (m,e1) in (m', Deref e')
                 (Assig (L l) e1) -> if(block (m, e1))
                                     then ((updateM ((L l),e1) m),Void)
                                     else let (m', e') = evals(m, e1)
                                          in
                                            evals(m', Assig (L l) e')
                 (Assig e1 e2) -> let (m', e') = evals(m, e1)
                                  in
                                    evals(m', Assig e' e2)
                 (Seq Void e2) -> (m, e2)
                 (Seq e1 e2) -> let (m',e') = evals (m, e1)
                                in
                                  evals(m', Seq e' e2)
                 w@(While e1 e2) -> evals(m, If (e1) (Seq e2 w) Void)

-- | eval. Devuelve la evaluación de un programa tal que eval e = e' syss
-- |       <ø,e> ->* <m',e'> y e' es un valor. En caso de que e' no sea un
-- |       valor deberá mostrar un mensaje de error particular al operador qu
-- |       lo causó.

eval :: Exp -> Exp
eval e = case evals ([],e) of
           (_, I n) -> I n
           (_, B b) -> B b
           (_, V _) -> error "[Var]"
           (_, Add _ _) -> error "[Add] Expects two Nat."
           (_, Mul _ _) -> error "[Mul] Expects two Nat."
           (_, Succ _) -> error "[Succ] Expects one Nat."
           (_, Pred _) -> error "[Pred] Expects one Nat."
           (_, Not _) -> error "[Not] Expects one Boolean."
           (_, And _ _) -> error "[And] Expects two Boolean."
           (_, Or _ _) -> error "[Or] Expects two Boolean."
           (_, Lt _ _) -> error "[Lt] Expects two Nat."
           (_, Gt _ _) -> error "[Gt] Expects two Nat."
           (_, Eq _ _) -> error "[Eq] Expects two Nat."
           (_, If _ _ _) -> error "[If] Expects one Boolean and two Exp."
           --lo quitamos para poder tener ciclo while true ¿?
           (_, Let _ _ _) -> error "[Let] Expects one Var and two Exp."
           (_, Void) -> Void
           (_, L l) -> L l
           (_, Alloc _) -> error "[Alloc] Expects one Exp."
           (_, Deref _) -> error "[Deref] Expects one Exp."
           (_, Assig _ _) -> error "[Deref] Expects one location and one Exp."
           (_, Seq _ _) -> error "[Seq] Expects two Exp."
           (_, While _ _) -> error "[While] Expects one condition and one Exp."

-- | expresión en miniC Que representa un programa con ciclo while.

predecesor n =
  evals([],Let "n" (Alloc n)(
          Seq(
              Seq(
                 Assig (V "p") (I 0))
               (
                 Assig (V "m")(I 0)))
          (
            Seq(
              While (Not (Eq (Deref (V "m")) (n)))
                 (
                   Seq(
                     (
                       Assig (V "p"))
                       (
                         Deref (V "m")))
                   (
                     Assig (V "m") (Add(Deref (V "m")) (I 1))))
              )(Deref (V "m")))
          )
       )

predChido n = (
  Let
     (
       "n"
     )(
      Alloc n
      )
      (
      Seq(
          Seq(
             Assig (V "p") (I 0)
             )(
             Assig (V "m") (I 0)
              )
         )(
          
          )
       )
              )

sumaDos n = eval(Add (I 2) n)

-- | block. Función que nos dice si una expresion está bloqueda o no.
block :: (Mem, Exp) -> Bool
block (m, (V _)) = True
block (m, (I _)) = True
block (m, (B _)) = True
block (m, (Add (I _) (I _))) = False
block (m, (Add a b)) = if(isNat (evals(m, a)) && isNat(evals(m, b)))
                       then False
                       else True
block (m, (Mul (I _) (I _))) = False
block (m, (Mul a b)) = if(isNat(evals(m, a)) && isNat(evals(m, b)))
                       then False
                       else True
block (m, (Succ (I _))) = False
block (m, (Succ a)) = if(isNat(evals(m, a)))
                      then False
                      else True
block (m, (Pred (I _))) = False
block (m, (Pred a)) = if(isNat(evals(m, a)))
                      then False
                      else True
block (m, (Not (B _))) = False
block (m, (Not x)) = if(esBool(evals(m, x)))
                     then False
                     else True
block (m, (And (B _) (B _))) = False
block (m, (And a b)) = if(esBool(evals(m, a)) && esBool(evals(m, b)))
                       then False
                       else True
block (m ,(Or (B _) (B _))) = False
block (m, (Or a b)) = if(esBool(evals(m, a)) && esBool(evals(m, b)))
                      then False
                      else True
block (m, (Lt (I _) (I _))) = False
block (m, (Lt a b)) = if(isNat(evals(m, a)) && isNat(evals(m, b)))
                      then False
                      else True
block (m, (Gt (I _) (I _))) = False
block (m ,(Gt a b)) = if(isNat(evals(m, a)) && isNat(evals(m, b)))
                      then False
                      else True
block (m, (Eq (I _) (I _))) = False
block (m, (Eq a b)) = if(isNat(evals(m, a)) && isNat(evals(m, b)))
                      then False
                      else True
block (m, (If (B _) _ _)) = False
block (m, (If b _ _)) = if(esBool(evals(m, b)))
                        then False
                        else True
block (m, (Let _ (I _) _)) = False
block (m, (Let _ (B _) _)) = False
block (m, (Let _ a _)) = if(isNat(evals(m, a)) || esBool(evals(m, a))) --Mmmm no lo sé Rick
                         then False
                         else True
block (m, (Void)) = True
block (m, (L _)) = True
block (m, (Alloc _)) = False
block (m, (Deref _)) = False
block (m, (Assig _ _)) = False
block (m, (Seq _ _)) = False
block (m, (While _ _)) = False

-- | tomaBool. Función que devuelve la parte booleana de una EAB.
tomaBool :: Exp -> Bool
tomaBool (B b) = b
tomaBool _ = error "No es un booleano"

-- | esBool. Función que nos dice si una EAB es un booleano.
esBool :: (Mem, Exp) -> Bool
esBool (m, (B _)) = True
esBool (m, _) = False

-- | isNat. Función que nos dice si una EAB es un natural.
isNat :: (Mem, Exp) -> Bool
isNat (m, (I _)) = True
isNat (m, _) = False

-- | tomaNat. Función que devuelve la parte natural de una EAB.
tomaNat :: Exp -> Int
tomaNat (I n) = n
tomaNat _ = error "no es un número"


--pruebas

--eval1
prueba1 = eval1([(L 0, B False)], (Add (I 1) (I 2)))

prueba2 = eval1([(L 0, B False)], (Let "x" (I 1) (Add (V "x") (I 2))))

prueba3 = eval1([(L 0, B False)], (Assig (L 0) (B True)))

prueba4 = eval1([], While (B True) (Add (I 1) (I 1)))

--evals
prueba5 = evals([], (Let "x" (Add (I 1) (I 2)) (Eq (V "x") (I 0))))

prueba6 = evals([], Add(Mul (I 2) (I 6)) (B True))

prueba7 = evals([], Assig(Alloc (B False)) (Add (I 1) (I 9)))

fact n =
  evals([],Let "x" ( Alloc n )(
           Let "y" ( Alloc ( I 1))(
               Seq
               (
                 While ( Gt (Deref (V "x")) (I 0))
                 ( Seq
                   ( Assig (V "y") (Mul (Deref (V "x"))(Deref (V "y"))))
                   ( Assig (V "x") (Add (Deref (V "x")) (I (-1))))
                 )
               )
               (
                 Deref (V "y")
               )
               )
           )
       )
  
