{-
- Lenguajes de Programación 2019-1
- Implementación de Postfix
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
- Kevin Ricardo Miranda Sanchez 314011163 kevinmiranda29@ciencias.unam.mx
-}



module Practica1 where

import Data.Typeable

-- Comandos 
data Command = I Int | ADD | DIV | Eq | EXEC | Gt | Lt | MUL | NGET | POP | REM |
               SEL | SUB | SWAP | ES [Command] deriving Show 

-- Constructor de la alabra reservada 
data PF = POSTFIX deriving (Show, Eq)

--representacion de los programas
type Program = (PF, Int, [Command])

-- pila de valores
type Stack = [Command]


-- arithOperation. Funcion que realiza las operaciones de los comandos aritmeticos. (add, div, eq, gt, lt, mul, rem, sub)
arithOperation :: Command -> Command -> Command -> Command
arithOperation (I n) (I m) com = case com of
                                   ADD -> I (m + n)
                                   DIV -> I (div n m)
                                   Eq  -> if n == m
                                          then I 1
                                          else I 0
                                   Gt  -> if n < m
                                          then I 1
                                          else I 0
                                   Lt  -> if m < n
                                          then I 1
                                          else I 0
                                   MUL -> I (m * n)
                                   REM -> I (mod n m)
                                   SUB -> I (m - n)
arithOperation _ _ _ = error "Error en tipo de datos :c"


-- | stackOperation. Funcion que realiza las operaciones de los comandos que alteran la pila de valores.
--(literal entera, nget, pop, sel,swap, secuencia ejecutable)
stackOperation :: Stack -> Command -> Stack
stackOperation s com = case com of
                         (I n) -> [(I n)] ++ s
                         ES xs -> [ES xs] ++ s
                         POP   -> if (validaStack s com)
                                  then cola s
                                  else error "Error en Stack, faltan elementos"
                         SWAP  -> if (validaStack s com)
                                  then swapiaux s
                                  else error "Error en Stack, faltan elementos"
                         SEL   -> if (validaStack s com)
                                  then selaux s
                                  else error "Error en Stack, faltan elementos"
                         NGET  -> ngetaux s
                         

-- | execOperation. Funcion que devuelve la lista de comandos y
-- la pila resultante de realizar la llamada a la operacion con exec.


-- | validProgram. Funcion que determina si la pila de valores que
--   se desea ejecutar con un programa es valida.



-- | executeCommands. Funci ́on que dada una lista de comandos y
--una pila de valores obtiene la pila de valores resultante despu ́es ejecutar
--todos los comandos.














{------------------------------------------------------------- --FUNCIONES AUXILIARES ---------------------------------------------------------------------------}
selaux :: Stack -> Stack
selaux (x:y:z:zs) = if (comToNat z == 0)
                    then (x:zs)
                    else (y:zs)

comToNat :: Command -> Int
comToNat (I n) = n
comToNat k = error "Tipo de dato no aceptado en SEL"

isNat :: Command -> Bool
isNat (I n) = True
isNat k = False


swapiaux :: Stack -> Stack
swapiaux (x:y:ys) = (y:x:ys)

cola :: [a] -> [a]
cola [] = []
cola (_:xs) = xs

ngetaux :: Stack -> Stack
ngetaux [] = []
--ngetaux (x:xs) = if (isNat x) then xs else xs ++ xs
ngetaux (x:xs) = if ( (isNat x) && (   (  1<=(comToNat x) && ( (comToNat x) <= len( ( xs )   ) &&  (isNat((x:xs) !! (comToNat x) ) ) ) )    )   )
              then  [((x:xs) !! (comToNat x))] ++ xs 
              else xs


validaStack :: Stack -> Command -> Bool
validaStack s com = case com of
                      POP   -> if (len s) > 0
                               then True
                               else False
                      SWAP  -> if (len s) > 1
                               then True
                               else False
                      SEL   -> if (len s) > 2
                               then True
                               else False

len :: Stack -> Int
len [] = 0
len (x:xs) = 1 + len xs

execOperation :: [Command] -> Stack -> ([Command], Stack)
execOperation = error " D: "
{-
execOperation 
-}

validProgram :: Program -> Stack -> Bool
validProgram = error " D: "

executeCommands :: [Command] -> Stack -> Stack
executeCommands = error " D: "

executeProgram :: Program -> Stack -> Int
executeProgram = error " D: "


