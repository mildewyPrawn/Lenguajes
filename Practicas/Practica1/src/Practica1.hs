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
execOperation :: [Command] -> Stack -> ( [ Command ] , Stack )
execOperation [] xs = ([ y| y <- xs, (isNat y) == False],  [x | x <- xs, isNat x]) 
execOperation (c:cs) p = case c of
  (I x) -> execOperation cs ((I x):p)
  ADD -> execOperation cs (p ++ [ADD])
  MUL -> execOperation cs (p ++ [MUL])
  DIV -> execOperation cs (p ++ [DIV])
  SUB -> execOperation cs (p ++ [SUB])
  REM -> execOperation cs (p ++ [REM])
  ES xs -> execOperation cs (xs)
 

-- | validProgram. Funcion que determina si la pila de valores que
--   se desea ejecutar con un programa es valida.
validProgram :: Program -> Stack -> Bool
validProgram (p,n,l) s = (valid p n l) && (n == length s)


-- | executeCommands. Funcion que dada una lista de comandos y
--una pila de valores obtiene la pila de valores resultante despu ́es ejecutar
--todos los comandos.
executeCommands :: [Command] -> Stack -> Stack
executeCommands [] p = p
executeCommands (c:cs) p = case c of
  (I x) -> executeCommands cs ((I x):p)
  ADD -> if (length p) >= 2 
    then executeCommands cs ((arithOperation ((fst (splitAt 2 p))!!1) ((fst (splitAt 2 p))!!0) c):(snd (splitAt 2 p))) 
    else error "Not enough numbers to add"
  MUL -> if (length p) >= 2 
    then executeCommands cs ((arithOperation (fst (splitAt 2 p)!!1) (fst (splitAt 2 p)!!0) c):snd (splitAt 2 p)) 
    else error "Not enough numbers to multiply" 
  DIV -> if (length p) >= 2 
    then executeCommands cs ((arithOperation (fst (splitAt 2 p)!!1) (fst (splitAt 2 p)!!0) c):snd (splitAt 2 p)) 
    else error "Not enough numbers to divide"
  SUB -> if (length p) >= 2 
    then executeCommands cs ((arithOperation (fst (splitAt 2 p)!!1) (fst (splitAt 2 p)!!0) c):snd (splitAt 2 p)) 
    else error "Not enough numbers to subtract"
  REM -> if (length p) >= 2 
    then executeCommands cs ((arithOperation (fst (splitAt 2 p)!!1) (fst (splitAt 2 p)!!0) c):snd (splitAt 2 p)) 
    else error "Not enough numbers to ..."
  Gt -> if (length p) >= 2 
    then executeCommands cs ((arithOperation (fst (splitAt 2 p)!!1) (fst (splitAt 2 p)!!0) c):snd (splitAt 2 p)) 
    else error "Not enough numbers to compare"
  Lt -> if (length p) >= 2 
    then executeCommands cs ((arithOperation (fst (splitAt 2 p)!!1) (fst (splitAt 2 p)!!0) c):snd (splitAt 2 p)) 
    else error "Not enough numbers to compare"
  Eq -> if (length p) >= 2 
    then executeCommands cs ((arithOperation (fst (splitAt 2 p)!!1) (fst (splitAt 2 p)!!0) c):snd (splitAt 2 p)) 
    else error "Not enough numbers to compare"
  SWAP -> if (length p) >= 2 
    then executeCommands cs ((stackOperation p c))
    else error "Error de ejecucion SWAP: Insuficientes argumentos."
  ES xs -> if (length p) >= 2 
    then executeCommands cs ((stackOperation p c))
    else error "Error de ejecucion ES: Insuficientes argumentos."
  POP -> if (length p) >= 2 
    then executeCommands cs ((stackOperation p c))
    else error "Error de ejecucion POP: Insuficientes argumentos."
  SEL -> if (length p) >= 2 
    then executeCommands cs ((stackOperation p c))
    else error "Error de ejecucion SEL: Insuficientes argumentos."
  NGET -> if (length p) >= 2 
    then executeCommands cs ((stackOperation p c))
    else error "Error de ejecucion NGET: Insuficientes argumentos."
  _ -> error "Terminar"










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


valid :: PF -> Int -> [Command] -> Bool
valid pf i xs = if (pf == POSTFIX && (i >=0 && (length xs) >= 2 )) then True else False

swapiaux :: Stack -> Stack
swapiaux (x:y:ys) = (y:x:ys)


cola :: [a] -> [a]
cola [] = []
cola (_:xs) = xs

ngetaux :: Stack -> Stack
ngetaux [] = []
ngetaux (x:xs) = if ( (isNat x) && (   (  1<=(comToNat x) && ( (comToNat x) <= length( ( xs )   ) &&  (isNat((x:xs) !! (comToNat x) ) ) ) )    )   )
              then  [((x:xs) !! (comToNat x))] ++ xs 
              else xs


validaStack :: Stack -> Command -> Bool
validaStack s com = case com of
                      POP   -> if (length s ) > 0
                               then True
                               else False
                      SWAP  -> if (length s) > 1
                               then True
                               else False
                      SEL   -> if (length s) > 2
                               then True
                               else False



executeProgram:: Program -> Stack -> [Command]
executeProgram (pf,i,k) s = if validProgram (pf,i,k) s then executeCommands k s else []
