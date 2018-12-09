{-
- Lenguajes de Programación 2019-1
- Representación anónima de términos lambda
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
-}

module Extra01 where

import Data.List

type Identifier = String

type Index = Int

type Substitution = (Index, ExprB)

data ExprL = VL Identifier
           | LL Identifier ExprL
           | AL ExprL ExprL deriving (Show)

data ExprB = IB Index
           | LB ExprB
           | AB ExprB ExprB deriving (Show)

-- | ctx. Función que obtiene el contexto canónico de una expresión.
ctx :: ExprL -> [Identifier]
ctx (VL v) = [v]
ctx (LL i ex) = [y | y <- ctx ex, y /= i]
ctx (AL e1 e2) = ctx e1 `union` ctx e2

--ctx (LL "x" (LL "y" (AL (VL "u" ) (AL (VL "x" ) (AL (VL "y" ) (AL (VL " z " ) (AL (VL " z " ) (AL (VL "y" ) (VL "v" ) ) ) ) ) ) ) ) )

--ctx (AL (VL "u" ) (AL (VL "v" ) (AL (VL "x" ) (AL (VL "y" )(AL (VL "z" ) (AL (VL "x" ) (AL (VL "x" ) (VL "v" ) ) ) ) ) ) ) )

-- | qn. Dado un contexto de índices y una expresión lambda obtiene su
-- |     representación anónima.
qn :: ([Identifier], ExprL) -> ExprB
qn (s, (VL i)) = IB (indice s i 0)
qn (s, (LL i e)) = LB (qn ([i]++s, e))
qn (s, (AL e1 e2)) = AB (qn (s, e1)) (qn (s, e2))

--qn ( [ "x" ] , LL " z " (AL (VL " z " ) (AL (VL "x" ) (LL "y" (AL (VL " z " )(AL (VL "x" ) (VL "y" ) ) ) ) ) ) )

--qn ( [ "y" , "x" ] , LL " z " (AL (VL " z " ) (AL (VL "x" ) (VL "y" ) ) ) )

-- | newVar. Dado un contexto de nombres, obtiene una nueva variable y la
-- |         agrega al contexto.
newVar :: [Identifier] -> [Identifier]
newVar l = case l of
             [] -> ["x"]
             (x:xs) -> let otro = incrVar x
                       in
                         if otro `elem` xs
                         then [x] ++ newVar xs
                         else [x, otro] ++ xs

-- | pn. Dado un contexto de nombres y una expresión anónima devuelve su
-- |     representación correspondiente en el cálculo lambda con nombres.
pn :: ([Identifier], ExprB) -> ExprL
pn (s, (IB i)) = VL (toma s i)
pn (s, (LB e)) = let new = newVar s
                     dif = new \\ s
                     x = toma dif 0
                 in
                   LL x (pn ([x] ++ s, e))
pn (s, (AB e1 e2)) = AL(pn (s, e1))(pn (s, e2))

--pn ( [ "x" ] , LB (AB ( IB 0 ) (AB ( IB 1 ) (LB (AB ( IB 1 ) (AB ( IB 2 )( IB 0 ) ) ) ) ) ) )

--pn ( [ "y" , "x" ] , LB (AB ( IB 0 ) (AB ( IB 2 ) ( IB 1 ) ) ) )

-- | shift. Desplaza los índices de una expresión anónima dado un parámetro de
-- |        corte.
shift :: (Int, Int, ExprB) -> ExprB
shift (d, c, (IB k)) = if k < c then (IB k) else IB (k + d)
shift (d, c, (LB t)) = LB (shift(d, c + 1, t))
shift (d, c, (AB r s)) = AB(shift(d, c, r))(shift(d,c,s))

--shift ( 1 , 0 , LB (AB ( IB 0 ) ( IB 2 ) ) )

--shift ( 1 , 2 , LB (AB ( IB 0 ) (AB ( IB 2 ) ( IB 1 ) ) ) )

-- | subst. Aplica la substitución a la expresión anónima.
subst :: ExprB -> Substitution -> ExprB
subst (IB n) (j, s) = if n == j then s else (IB n)
--subst (LB t) (j, s) = subst (LB t) (j + 1, shift(1, 0, s))
subst (LB t) (j, s) = LB(subst t (j + 1, shift(1, 0, s)))
subst (AB t r) (j, s) = AB(subst t (j, s))(subst r (j, s))

--subst (LB (AB ( IB 0 ) (AB ( IB 2 ) ( IB 1 ) ) ) ) ( 1 , LB (AB ( IB 0 )( IB 2 ) ) )

-- | eval1. Aplica un paso de la reducción de una expresión anónima.
eval1 :: ExprB -> ExprB
eval1 (AB (LB t)s) = shift(-1, 0, subst t (0, shift (1, 0, s)))
eval1 (LB t) = LB (eval1 t)
eval1 (AB t1 t2) = AB (t1) (eval1 t2)
eval1 (IB n) = (IB n)

--eval1 (AB (LB (AB (LB ( IB 1 ) ) ( IB 0 ) ) ) (LB (AB ( IB 2 ) (AB ( IB 1 )( IB 0 ) ) ) ) )

-- | locked. Determina si una expresión anónima está bloqueada es decir, no se
-- |         pueden hacer más reducciones.
locked :: ExprB -> Bool
locked (IB _) = True
locked (AB (LB e1) e2) = False
locked (AB e1 e2) = locked e1 && locked e2
locked (LB e) = locked e

--locked ( IB 1 )

--locked (LB (AB (LB ( IB 0 ) ) (LB ( IB 0 ) ) ) )

-- | eval. Evalúa una expresión anónima hasta quedar bloqueada.
eval :: ExprB -> ExprB
eval e = let e1 = eval1 e
         in
           if locked e1
           then e1
           else eval e1

---------------------------------------------------------------------------
---                           FUNCIONES AUXILIARES                      ---
---------------------------------------------------------------------------

-- | incrVar. Dado un identificador, si este no termina en número, le agrega
-- |          el sufijo '1', en caso contrario toma el valor del número y lo
-- |          incrementa en 1.
incrVar :: Identifier -> Identifier
incrVar xs = if (elem (last xs) (['a'..'z']++['A'..'Z']))
             then xs ++ show 1
             else init xs ++ show((read[last xs])+1)

-- | indice. Dada una lista, y un elemento, regresa la posición del elemento.
indice :: (Eq a) => [a] -> a -> Int -> Int
indice [] _ _ = error "No esta en la lista"
indice (x:xs) y acc = if x == y
                      then acc
                      else indice xs y (acc + 1)

-- | toma. FUnción que toma el i-esimo elemento de una lista.
toma :: [a] -> Int -> a
toma [] _ = error "No hay suficientes elementos"
toma (x:xs) 0 = x
toma (x:xs) n = toma xs (n - 1)
