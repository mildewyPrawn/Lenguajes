{-
- Lenguajes de Programación 2019-1
- Expresiones KEAB
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
- Kevin Ricardo Miranda Sanchez 314011163 kevinmiranda29@ciencias.unam.mx
-}

module KEAB where

import Data.List

type Identifier = String

type Substitution = (Identifier, Expr)

type Pending = ()

type Stack = [Frame]

data State = E(Stack, Expr)
           | R(Stack, Expr)

data Expr = V Identifier | I Int  | B Bool
  | Add Expr Expr | Mul Expr Expr | Succ Expr | Pred Expr
  | And Expr Expr | Or Expr Expr  | Not Expr
  | Lt Expr Expr  | Gt Expr Expr  | Eq Expr Expr
  | If Expr Expr Expr
  | Let Identifier Expr Expr
  | LetCC Identifier Expr
  | Continue Expr Expr
  | Cont Stack deriving (Eq)

data Frame = AddL Pending Expr
           | AddR Expr Pending
           | MulL Pending Expr
           | MulR Expr Pending
           | SuccF Pending
           | PredF Pending
           | AndL Pending Expr
           | AndR Expr Pending
           | OrL Pending Expr
           | OrR Expr Pending
           | NotF Pending
           | LtL Pending Expr
           | LtR Expr Pending
           | GtL Pending Expr
           | GtR Expr Pending
           | EqL Pending Expr
           | EqR Expr Pending
           | IfF Pending Expr Expr
           | LetF Identifier Pending Expr
           | ContinueL Pending Expr
           | ContinueR Expr Pending deriving (Eq)

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
    (Continue a b) -> "Continue(" ++ (show a) ++ ", " ++ (show b) ++ ")"

instance Show Frame where
  show e = case e of
    (AddL _ b) -> "Add( _ " ++ ", " ++ (show b) ++ ")"
    (AddR a _) -> "Add(" ++ (show a) ++ ", " ++ " _ )"
    (MulL _ b) -> "Mul( _ " ++ ", " ++ (show b) ++ ")"
    (MulR a _) -> "Mul(" ++ (show a) ++ ", " ++ " _ )"
    (SuccF _) -> "Succ( _ )"
    (PredF _) -> "Pred( _ )"
    (NotF _) -> "Not( _ )"
    (AndL _ b) -> "And( _ " ++ ", " ++ (show b) ++ ")"
    (AndR a _) -> "And(" ++ (show a) ++ ", " ++ " _ )"
    (OrL _ b) -> "Or( _ " ++ ", " ++ (show b) ++ ")"
    (OrR a _) -> "Or(" ++ (show a) ++ ", " ++ " _ )"
    (LtL _ b) -> "Lt( _ " ++ ", " ++ (show b) ++ ")"
    (LtR a _) -> "Lt(" ++ (show a) ++ ", " ++ " _ )"
    (GtL _ b) -> "Gt( _ " ++ ", " ++ (show b) ++ ")"
    (GtR a _) -> "Gt(" ++ (show a) ++ ", " ++ " _ )"
    (EqL _ b) -> "Eq( _ " ++ ", " ++ (show b) ++ ")"
    (EqR a _) -> "Eq(" ++ (show a) ++ ", " ++ " _ )"
    (IfF _ a b) -> "If( _ " ++ (show a) ++ ", " ++ (show b) ++ ")"
    (LetF x _ b) -> "Let(" ++ (show x) ++ ", _, " ++ (show b) ++ ")"
    (ContinueL _ b) -> "Continue( _ " ++ ", " ++ (show b) ++ ")"
    (ContinueR a _) -> "Continue(" ++ (show a) ++ ", " ++ " _ )"

instance Show State where
  show e = case e of
    E(s, ex) -> (show s) ++ " ≻ " ++ (show ex)
    R(s, ex) -> (show s) ++ " ≺ " ++ (show ex)

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
frVars (LetCC x e) = [y | y <- frVars e, y /= x]
frVars (Continue a b) = frVars a `union` frVars b
frVars (Cont s) = []

-- | subst. Realiza la substitución de una expresión de EAB.
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
subst (LetCC x e1) (y, e) = if (elem x ([y] ++ frVars e))
                           then error "Could not apply the substitution"
                           else LetCC x (subst e1 (y,e))
subst (Continue a b) s = Continue (subst a s) (subst b s)
subst (Cont _) _ = error "Could not apply the substitution"

-- | eval1. Recibe un estado de la máquina K, y devuelve un paso de la
-- |        transición.
eval1 :: State -> State
eval1 (E (s, I n)) = (R (s, I n))
eval1 (E (s, B b)) = (R (s, B b))
eval1 (E (s, V v)) = (R (s, V v))
eval1 (E (s, Succ e1)) = (E ((SuccF ()):s, e1))
eval1 (R ((SuccF ()):s, (I n))) = (R (s, I (n + 1)))
eval1 (E (s, Pred e1)) = (E ((PredF ()):s, e1))
eval1 (R ((PredF ()):s, (I n))) = (R (s, I (n - 1)))
eval1 (E (s, Not e1)) = (E ((NotF ()):s, e1))
eval1 (R ((NotF ()):s, (B b))) = (R (s, B (not b)))
eval1 (E (s, Add e1 e2)) = (E ((AddL () e2):s, e1))
eval1 (R ((AddL () e2):s, I n)) = (E ((AddR (I n) ()):s, e2))
eval1 (R ((AddR (I n) ()):s, I n')) = (R (s, I (n + n')))
eval1 (E (s, Mul e1 e2)) = (E ((MulL () e2):s, e1))
eval1 (R ((MulL () e2):s, I n)) = (E ((MulR (I n)()):s, e2))
eval1 (R ((MulR (I n) ()):s, I n')) = (R (s, I (n * n')))
eval1 (E (s, And e1 e2)) = (E ((AndL () e2):s, e1))
eval1 (R ((AndL () e2):s, B b)) = (E ((AndR (B b) ()):s, e2))
eval1 (R ((AndR (B b) ()):s, B b')) = (R (s, B (b && b')))
eval1 (E (s, Or e1 e2)) = (E ((OrL () e2):s, e1))
eval1 (R ((OrL () e2):s, B b)) = (E ((OrR (B b) ()):s, e2))
eval1 (R ((OrR (B b) ()):s, B b')) = (R (s, B (b || b')))
eval1 (E (s, Lt e1 e2)) = (E ((LtL () e2):s, e1))
eval1 (R ((LtL () e2):s, I n)) = (E ((LtR (I n) ()):s, e2))
eval1 (R ((LtR (I n) ()):s, I n')) = (R (s, B (n < n')))
eval1 (E (s, Gt e1 e2)) = (E ((GtL () e2):s, e1))
eval1 (R ((GtL () e2):s, I n)) = (E ((GtR (I n) ()):s, e2))
eval1 (R ((GtR (I n) ()):s, I n')) = (R (s, B (n' < n)))
eval1 (E (s, Eq e1 e2)) = (E ((EqL () e2):s, e1))
eval1 (R ((EqL () e2):s, I n)) = (E ((EqR (I n) ()):s, e2))
eval1 (R ((EqR (I n) ()):s, I n')) = (R (s, B (n' == n)))
eval1 (E (s, If b e1 e2)) = (E ((IfF () e1 e2):s, b))
eval1 (R ((IfF () e1 _):s, (B True))) = (E (s, e1))
eval1 (R ((IfF () _ e2):s, (B False))) = (E (s, e2))
eval1 (E (s, Let x e1 e2)) = (E ((LetF x () e2):s, e1))
eval1 (R ((LetF x () e2):s, v)) = (E (s, subst e2 (x,v)))
--eval1 _ = (E ([], Error))
--nuevos.
eval1 (E (s, Cont r)) = (R (s, Cont r))
eval1 (E (s, LetCC x e)) = (E (s, subst e (x, Cont(s))))
eval1 (E (s, Continue e1 e2)) = (E ((ContinueL () e2):s, e1))
eval1 (R ((ContinueL () e2):s, ( n))) = (E ((ContinueR ( n) ()):s, e2))
--eval1 (R ((ContinueL () e2):s, (B b))) = (E ((ContinueR (B b) ()):s, e2))
eval1 (R ((ContinueR (Cont s') ()):s, ( n))) = (R (s', ( n)))
--eval1 (R ((ContinueR (Cont s') ()):_, (B b))) = (R (s', (B b)))


-- | evals. Recibe un estado de la máquina K y devuelve un estado derivado de
-- |        evaluar varias veces hasta obtener la pila vacía.
evals :: State -> State
evals (E ([], I n)) = (R ([], I n))
evals (E ([], B b)) = (R ([], B b))
evals (E ([], V v)) = (R ([], V v))
evals (R ([], I n)) = (R ([], I n))
evals (R ([], B b)) = (R ([], B b))
evals (R ([], V v)) = (R ([], V v))
evals otra = evals(eval1 otra)

-- | eval. Recibe una expresión EAB, la evalúa con la máquina K, y devuelve un
-- |       valor, iniciando con la pila vacía está devuelve un valor a la pila.
eval :: Expr -> Expr
eval e = let
  x = evals(E ([], e))
  in
    let ex = takeExpr x
    in
      ex
      {-
      if ex == Error
      then error "No se pudo evaluar."
      else ex-}

-- | takeExpr. Función auxiliar que obtiene el lado derecho de una máquina K.
takeExpr :: State -> Expr
takeExpr (E (_ ,e)) = e
takeExpr (R (_ ,e)) = e


