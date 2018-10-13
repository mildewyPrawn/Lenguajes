{-
- Lenguajes de Programación 2019-1
- El lenguaje EAB
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
-}

module EjerSem02 where

import Data.List

type Identifier = String

data Exp = V Identifier | I Int | B Bool
         | Add Exp Exp | Mul Exp Exp | Succ Exp | Pred Exp
         | Not Exp | And Exp Exp | Or Exp Exp
         | Lt Exp Exp | Gt Exp Exp | Eq Exp Exp
         | If Exp Exp Exp
         | Let Identifier Exp Exp deriving (Eq)

-- | Instancia de la clase Show.
instance Show Exp where
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
    (Or a b) -> "And(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Lt a b) -> "Lt(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Gt a b) -> "Gt(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (Eq a b) -> "Eq(" ++ (show a) ++ ", " ++ (show b) ++ ")"
    (If p a b) -> "If(" ++ "," ++ (show p) ++ "," ++ (show a) ++ (show b) ++ ")"
    (Let x a b) -> "Let(" ++ (show x) ++ "," ++ (show a) ++ "." ++ (show b)
                   ++ ")"

type Substitution = (Identifier, Exp)

-- | frVars. Función que obtiene el conjunto de variables libres de una expresion.
frVars :: Exp -> [Identifier]
frVars (V x) = [x]
frVars (I _) = []
frVars (B _) = []
-- cambiar los ++ por 'union'
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

-- | subst. Función que aplica la substitucion a la expresión dada en caso de ser
--          posible.
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
                     {-
subst (Let z p q) (y, e) = if notElem z ([y] ++ frVars e)
                           then Let (z)(subst p (y, e))(subst q (y, e))
                           else err
-}


-- | alphaEq. Función que determina si dos expresiones son alfa-equivalentes.
alphaEq :: Exp -> Exp -> Bool
alphaEq (V x) (V y) = x == y -- cambiar por x == y
alphaEq (I x) (I y) = x == y
alphaEq (B x) (B y) = x == y
-- alphaEq (Add a1 a2) (Add b1 b2) = (alphaEq a1 b1) && (alphaEq a2 b2)
alphaEq (Add a1 a2) (Add b1 b2) = (alphaEq a1 b1) && (alphaEq a2 b2)
alphaEq (Mul a1 a2) (Mul b1 b2) = (alphaEq a1 b1) && (alphaEq a2 b2)
alphaEq (Succ a1) (Succ b1) = (alphaEq a1 b1)
alphaEq (Pred a1) (Pred b1) = (alphaEq a1 b1)
alphaEq (Not a1) (Not b1) = (alphaEq a1 b1)
alphaEq (And a1 a2) (And b1 b2) = (alphaEq a1 b1) && (alphaEq a2 b2)
alphaEq (Or a1 a2) (Or b1 b2) = (alphaEq a1 b1) && (alphaEq a2 b2)
alphaEq (Lt a1 a2) (Lt b1 b2) = (alphaEq a1 b1) && (alphaEq a2 b2)
alphaEq (Gt a1 a2) (Gt b1 b2) = (alphaEq a1 b1) && (alphaEq a2 b2)
alphaEq (Eq a1 a2) (Eq b1 b2) = (alphaEq a1 b1) && (alphaEq a2 b2)
alphaEq (If b t f) (If b1 t1 f1) = (alphaEq b1 b) &&
                                   (alphaEq t t1) && (alphaEq f f1)
{-alphaEq (Let x a b) (Let y c d) =
  ((V x) == (subst(V y) (y , (V x))))  &&
  a == (subst c (y, (a))) &&
  b == (subst d (y, (b))) &&
  length(frVars(Let x a b)) == length(frVars(Let y c d))-}
alphaEq (Let x a1 a2) (Let y b1 b2) = alphaEq a1 b1 && alphaEq (subst a2 (x,a1)) (subst b2 (y, b1))
alphaEq _ _ = False

--------------------------------------------------------------------------------
--------                             Pruebas                            --------
--------------------------------------------------------------------------------
frVars1 = frVars (Add (V "x") (I 5))
--Resultado: ["x"]

frVars2 = frVars (Let "x" (I 1) (V "x"))
--Resultado: []

subst1 = subst (Add (V "x") (I 5)) ("x", I 10)
--Resultado: Add(I 10) (I 5) ::: NOTA ::: no se toma en cuenta el instance Show

subst2 = subst (Let "x" (I 1) (V "x")) ("y", Add (V "x") (I 5))
--Resultado: ***Exception: Could not apply the substitucion

alphaEq1 = alphaEq (Let "x" (I 1) (V "x")) (Let "y" (I 1) (V "y"))
--Resultado: True

alphaEq2 = alphaEq (Let "x" (I 1) (Add (V "x") (V "x")))
           (Let "y" (I 1) (Add (V "y") (V "z")))
--Resultado: False
