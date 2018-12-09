{-
- Lenguajes de Programación 2019-1
- Semantica de paso grande
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
-}

module Extra02 where

import Data.List

type Identifier = String

type Substitution = (Identifier, Expr)

type Decl = (Identifier, Type)

type TypCtx = [Decl]

data Type = Integer
          | Boolean
          | Prod Type Type
          | Func Type Type deriving (Eq)

data Expr = V Identifier | I Int | B Bool
          | Add Expr Expr | Mul Expr Expr | Succ Expr | Pred Expr
          | Not Expr | And Expr Expr | Or Expr Expr
          | Lt Expr Expr | Gt Expr Expr | Eq Expr Expr
          | If Expr Expr Expr
          | Let Identifier Expr Expr
          | Pair Expr Expr
          | Fst Expr | Snd Expr
          | Lam Identifier Expr
          | App Expr Expr
          | Rec Identifier Expr deriving (Eq)

instance Show Expr where
  show e = case e of
    (V x) -> "V[" ++ x ++ "]"
    (I n) -> "N[" ++ (show n) ++ "]"
    (B b) -> "B[" ++ (show b) ++ "]"
    (Add a b) -> "Add(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Mul a b) -> "Mul(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Succ x) -> "Succ(" ++ (show x) ++ ")"
    (Pred x) -> "Pred(" ++ (show x) ++ ")"
    (Not x) -> "Not(" ++ (show x) ++ ")"
    (And a b) -> "And(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Or a b) -> "Or(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Lt a b) -> "Lt(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Gt a b) -> "Gt(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Eq a b) -> "Eq(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (If p a b) -> "If(" ++ (show p) ++ ", " ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Let x a b) -> "Let(" ++ (show x) ++ "," ++ (show a) ++ "." ++ (show b)
    (Pair a b) -> "Pair < " ++ (show a) ++ ", " ++ (show b) ++ " >"
    (Fst a) -> "Fst(" ++ (show a) ++ ")"
    (Snd a) -> "Snd(" ++ (show a) ++ ")"
    (Lam x ex) -> "\\" ++ x ++ " -> " ++ (show ex)
    (App a b) -> "(" ++ (show a) ++ ")" ++ "(" ++ (show b) ++ ")"
    (Rec i ex) -> "Rec( " ++ (show i) ++ ".(" ++ (show ex) ++ "))"

-- | frVars. Obtiene el conjunto de variables libres de una expresion.
frVars :: Expr -> [Identifier]
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
frVars (Pair a b) = frVars a `union` frVars b
frVars (Fst a) = frVars a
frVars (Snd b) = frVars b
frVars (Lam x e) = [y | y <- (frVars e) , y/=x ]
frVars (App e1 e2) = [x | x <- (frVars e1 `union` frVars e2)]
frVars (Rec i ex) = [y | y <- (frVars ex) , y/=i ]

-- | subst. Aplica una sustitución.
subst :: Expr -> Substitution -> Expr
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
subst (Pair a b) s = Pair(subst a s)(subst b s)
subst (Fst a) s = Fst(subst a s)
subst (Snd b) s = Snd(subst b s)
subst (Lam x e) (i,s)
  | x == i = (Lam x e)
  | x `elem` (frVars s) = Lam (incrVar x) (subst e (i,s))
  | otherwise = Lam x (subst e (i,s))
subst (App e e1) (i,s) = App (subst e (i,s)) (subst e1 (i,s))
subst (Rec x e) (i,s)
  | x == i = (Rec x e)
  | x `elem` (frVars s) = Rec (incrVar x) (subst e (i,s))
  | otherwise = Rec x (subst e (i,s))

-- NOTA: expresion cerrada sino contiene variables libres

-- | evals. Devuelve la evaluación de una expresión implementando las reglas
-- |        anteriores(PDF).
evals :: Expr -> Expr
evals (V _) = error "Ya no se puede evaluar."
evals (I n) = (I n)
evals (B b) = (B b)
evals e@(Pair a b) = if (canonica e)
                     then e
                     else Pair(evals a)(evals b)
evals a@(Lam x e) = if (canonica a)
                    then a
                    else (Lam x (evals e))
evals (Pred a) = let n = evals a
                 in
                   if isNat n
                   then I ((tomaNat n) - 1)
                   else error "No es un numero"
evals (Succ a) = let n = evals a
                 in
                   if isNat n
                   then I ((tomaNat n) + 1)
                   else error "No es un numero"

evals (Add a b) = let n1 = evals a
                      n2 = evals b
                  in
                    if isNat n1 && isNat n2
                    then I (tomaNat n1 + tomaNat n2)
                    else error "No es un numero"
evals (Mul a b) = let n1 = evals a
                      n2 = evals b
                  in
                    if isNat n1 && isNat n2
                    then I (tomaNat n1 * tomaNat n2)
                    else error "No es un numero"
evals (Lt a b) = let n1 = evals a
                     n2 = evals b
                  in
                    if isNat n1 && isNat n2
                    then B (tomaNat n1 < tomaNat n2)
                    else error "No es un numero"
evals (Gt a b) = let n1 = evals a
                     n2 = evals b
                  in
                    if isNat n1 && isNat n2
                    then B (tomaNat n1 > tomaNat n2)
                    else error "No es un numero"
evals (Eq a b) = let n1 = evals a
                     n2 = evals b
                  in
                    if isNat n1 && isNat n2
                    then B (tomaNat n1 == tomaNat n2)
                    else error "No es un numero"
evals (Not e) = let b = evals e
                in
                  if isBool b
                  then B (not (tomaBool b))
                  else error "No es un booleano"
evals (And a b) = let b1 = evals a
                      b2 = evals b
                  in
                    if isBool b1 && isBool b2
                    then B (tomaBool b1 && tomaBool b2)
                    else error "No es un booleano"
evals (Or a b) = let b1 = evals a
                     b2 = evals b
                 in
                   if isBool b1 && isBool b2
                   then B (tomaBool b1 || tomaBool b2)
                   else error "No es un booleano"
evals (If b e1 e2) = let con = evals b
                     in
                       if isBool con
                       then if tomaBool con
                            then evals e1
                            else evals e2
                       else error "NO es un booleano"
evals (Let x e1 e2) = let c1 = evals e1
                          c2 = evals (subst e2 (x, c1))
                      in
                        c2
evals (Fst e) = let c = evals e
                in
                  if isPair c
                  then tomaFst c
                  else error "No es un par"
evals (Snd e) = let c = evals e
                in
                  if isPair c
                  then tomaSnd c
                  else error "No es un par"
evals (App e1 e2) = let o@(Lam x e1') = evals e1
                        c2 = evals e2
                        c = evals (subst o (x, c2))
                    in
                      c
evals (Rec x e) = Rec x e

-- | vt. Función que verifica el tipado de un programa tal que vt Γ e T = True
-- |     syss Γ ⊢ e:T.
vt :: TypCtx -> Expr -> Type -> Bool
vt [] (V _) _ = False
vt ((a,b):xs) (V x) t = if x == a
                        then b == t
                        else vt xs (V x) t
vt _ (I _) t = t == Integer
vt _ (B _) t = t == Boolean
vt s (Add e1 e2) t = t == Integer &&
                     vt s e1 t &&
                     vt s e2 t
vt s (Mul e1 e2) t = t == Integer &&
                     vt s e1 t &&
                     vt s e2 t
vt s (Succ e) t = t == Integer &&
                  vt s e t
vt s (Pred e) t = t == Integer &&
                  vt s e t
vt s (And e1 e2) t = t == Boolean &&
                     vt s e1 t &&
                     vt s e2 t
vt s (Or e1 e2) t = t == Boolean &&
                    vt s e2 t &&
                    vt s e1 t
vt s (Not e) t = t == Boolean &&
                 vt s e t
vt s (Lt e1 e2) t = t == Boolean &&
                    vt s e1 Integer &&
                    vt s e2 Integer
vt s (Gt e1 e2) t = t == Boolean &&
                    vt s e1 Integer &&
                    vt s e2 Integer
vt s (Eq e1 e2) t = t == Boolean &&
                    vt s e1 Integer &&
                    vt s e2 Integer
vt s (If b e1 e2) t = vt s b Boolean &&
                      vt s e1 t &&
                      vt s e2 t
vt s (Let x e1 e2) t = vt s (subst e2 (x, e1)) t
vt s (Pair a b) (Prod t1 t2) = vt s a t1 && vt s b t2
vt s (Fst a) t = vt s a t
vt s (Snd a) t = vt s a t
vt [] (Lam x e) (Func t1 t2) = error "No hay tipo para x"
vt s@((v, bi):xs) l@(Lam x e) t@(Func t1 t2) = if (v == x)
                                           then bi == t1 && vt s e t2
                                           else vt xs l t

-- | eval. Función que devuelve la evaluación de una expresión solo si está
-- |       bien tipada.
eval :: Expr -> Expr
eval c@(If b e1 e2) = let con = evals b
                          e1' = evals e1
                          e2' = evals e2
                          st = sameType e1' e2'
                      in
                        if (isBool con)
                        then if st then evals c else error "Invalid Expresion"
                        else error "No es un booleano"
eval otro = evals otro

---------------------------------------------------------------------------
---                           FUNCIONES AUXILIARES                      ---
---------------------------------------------------------------------------
-- | incrVar. Dado un identificador, si este no termina en número, le agrega
-- |          el sufijo '1', en caso contrario toma el valor del número y lo
-- |          incrementa en 1.

--SE HIZO PARA HACER SUSTITUCIONES
incrVar :: Identifier -> Identifier
incrVar xs = if (elem (last xs) (['a'..'z']++['A'..'Z']))
             then xs ++ show 1
             else init xs ++ show((read[last xs])+1)

sameType :: Expr -> Expr -> Bool
sameType (I _) (I _) = True
sameType (B _) (B _) = True
sameType _ _ = False

-- | isPair. Función que nos dice si una Expr es un par.
isPair :: Expr -> Bool
isPair (Pair _ _) = True
isPair _ = False

-- | tomaFst. Función que toma la primer entrada de un par.
tomaFst :: Expr -> Expr
tomaFst (Pair a _) = a
tomaFst _ = error "No es un par desde tomaFst"

-- | tomaSnd. Función que toma la segunda entrada de un par.
tomaSnd :: Expr -> Expr
tomaSnd (Pair _ b) = b
tomaSnd _ = error "No es un par desde tomaSnd"

-- | isBool. Función que nos dice si una Expr es de tipo Bool.
isBool :: Expr -> Bool
isBool (B _) = True
isBool _ = False

-- | tomaBool. Función que toma el valor de un tipo Bool
tomaBool :: Expr -> Bool
tomaBool (B b) = b
tomaBool _ = error "No es un booleano desde tomaBool"

-- | isNat. Función que nos dice una Expr es de tipo Nat.
isNat :: Expr -> Bool
isNat (I _) = True
isNat _ = False

-- | tomaNat. Función que toma el valor de un tipo Nat
tomaNat :: Expr -> Int
tomaNat (I n) = n
tomaNat _ = error "No es un numero desde tomaNat"

-- | canonica. Función que calcula el conjunto C (formas canónicas) de una Expr.
canonica :: Expr -> Bool
canonica (I _) = True
canonica (B _) = True
canonica (Pair a b) = canonica a && canonica b
canonica a@(Lam x e) = frVars a == []
canonica _ = False
