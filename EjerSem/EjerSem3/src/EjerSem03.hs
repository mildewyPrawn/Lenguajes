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

data Exp = V Identifier | I Int | B Bool | Void | L Int
         | Add Exp Exp | Mul Exp Exp | Succ Exp | Pred Exp
         | Not Exp | And Exp Exp | Or Exp Exp
         | Lt Exp Exp | Gt Exp Exp | Eq Exp Exp
         | If Exp Exp Exp
         | Let Identifier Exp Exp
         | Alloc Exp
         | Deref Exp
         | Assig Exp
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

-- | Dada una memoria, obtiene todas sus direcciones de memoria.
domain :: Mem -> [Int]
domain m = case m of
             [] -> []
             (L l, _):xs -> [l] ++ domain xs
             (_, _):xs -> error "Corrupted memory."

-- | Dada una memoria, genera una nueva dirección de memoria, que no esté
-- | contenida.
newL :: Mem -> Addr
newL m = case m of
           [] -> L 0
           (L l, o):xs -> if(l+1 `elem` domain m)
                          then newL xs
                          else L (l +
                                  1)

-- | Dada una dirección de memoria, devuelve el valor contenido en la celda,
-- | en caso de no encontrarla devuelve Nothing
accessM :: Addr -> Mem -> Maybe Value
accessM _ [] = Nothing
accessM (L l) ((L l', o):xs) = if l == l'
                               then Just o
                               else accessM (L l) xs
accessM (L l) ((_, o):xs) = error "Corrupted memory."

-- | Dada una celda de memoria, actualiza el valor de esta misma en la memoria.
updateM :: Cell -> Mem -> Mem
updateM _ [] = error "Memory address does not exist."
updateM a@(L l, I n) (b@(L l', I _):xs) = if l == l'
                                          then [(L l, I n)] ++ xs
                                          else [b] ++ updateM a xs
updateM a@(L l, B n) (b@(L l', B _):xs) = if l == l'
                                          then [(L l, B n)] ++ xs
                                          else [b] ++ updateM a xs
updateM (L _, _) ((L l', B _):xs) = error "Memory can only store values."
updateM (L _, _) ((_ , _):xs) = error "Corrupted memory."

-- | 
