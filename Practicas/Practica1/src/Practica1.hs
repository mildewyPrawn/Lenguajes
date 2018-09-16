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


-- | arithOperation. Funcion que realiza las operaciones de los comandos aritmeticos. (add, div, eq, gt, lt, mul, rem, sub)
arithOperation :: Command -> Command -> Command -> Command
arithOperation (I n) (I m) com = case com of
                                   ADD -> I (m + n)
                                   DIV -> if m == 0
                                          then error "Division entre 0"
                                          else I (div n m)
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
                                   --SUB -> I (m - n) -- ¿ERROR EN RESTA?'
                                   SUB -> I (n - m) -- ¿ERROR EN RESTA?'
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
                         NGET  -> if(validaStack s com)
                                  then ngetaux s
                                  else error "Error en Stack, faltan elementos"

-- | execOperation. Funcion que devuelve la lista de comandos y
-- la pila resultante de realizar la llamada a la operacion con exec.
execOperation :: [Command] -> Stack -> ( [ Command ] , Stack )
execOperation c ((ES x):y) = (x++c,y)
execOperation _ _ = error "UPS"

-- | validProgram. Funcion que determina si la pila de valores que
--   se desea ejecutar con un programa es valida.
validProgram :: Program -> Stack -> Bool
validProgram (pf,n,l) s = (valid pf n l) && (n == length s) && (litS s)

-- | executeCommands. Funcion que dada una lista de comandos y
--una pila de valores obtiene la pila de valores resultant's después ejecutar
--todos los comandos.
executeCommands :: [Command] -> Stack -> Stack
executeCommands [] s = s
executeCommands (c:cs) s = case c of
                             (I x) -> executeCommands cs ((I x):s)
                             ADD -> if (length s) >= 2
                               then executeCommands cs ((arithOperation ((fst (splitAt 2 s))!!1) ((fst (splitAt 2 s))!!0) c):(snd (splitAt 2 s)))
                               else error "Not enough numbers to add"
                             MUL -> if (length s) >= 2
                               then executeCommands cs ((arithOperation (fst (splitAt 2 s)!!1) (fst (splitAt 2 s)!!0) c):snd (splitAt 2 s))
                               else error "Not enough numbers to multiply"
                             DIV -> if (length s) >= 2
                               then executeCommands cs ((arithOperation (fst (splitAt 2 s)!!1) (fst (splitAt 2 s)!!0) c):snd (splitAt 2 s))
                               else error "Not enough numbers to divide"
                             SUB -> if (length s) >= 2
                               then executeCommands cs ((arithOperation (fst (splitAt 2 s)!!1) (fst (splitAt 2 s)!!0) c):snd (splitAt 2 s))
                               else error "Not enough numbers to subtract"
                             REM -> if (length s) >= 2
                                    then executeCommands cs ((arithOperation (fst (splitAt 2 s)!!1) (fst (splitAt 2 s)!!0) c):snd (splitAt 2 s))
                                    else error "Not enough numbers to ..."
                             Gt -> if (length s) >= 2
                                   then executeCommands cs ((arithOperation (fst (splitAt 2 s)!!1) (fst (splitAt 2 s)!!0) c):snd (splitAt 2 s))
                                   else error "Not enough numbers to compare"
                             Lt -> if (length s) >= 2
                                   then executeCommands cs ((arithOperation (fst (splitAt 2 s)!!1) (fst (splitAt 2 s)!!0) c):snd (splitAt 2 s))
                                   else error "Not enough numbers to compare"
                             Eq -> if (length s) >= 2
                                   then executeCommands cs ((arithOperation (fst (splitAt 2 s)!!1) (fst (splitAt 2 s)!!0) c):snd (splitAt 2 s))
                                   else error "Not enough numbers to compare"
                             SWAP -> if (length s) >= 2
                                     then executeCommands cs ((stackOperation s c))
                                     else error "Not enough arguments to swap."
                             (ES xs) -> executeCommands cs (stackOperation s (ES xs))
                             EXEC ->  executeCommands (execaux s ++ cs) (cola s)
                             POP -> if (length s) >= 2
                                    then executeCommands cs ((stackOperation s c))
                                    else error "Not enough arguments."
                             SEL -> if (length s) >= 2
                                    then executeCommands cs ((stackOperation s c))
                                    else error "Not enough arguments."
                             NGET -> if (length s) >= 2
                                     then executeCommands cs ((stackOperation s c))
                                     else error "Not enough arguments."

  -- | executeProgram. Función que ejecuta cualquier programa en Postfix.
executeProgram :: Program -> Stack -> [Command]
executeProgram (pf, i, k) s = if validProgram (pf,i,k) s
                              then executeCommands k s
                              else error "NO es un programa valido."

{-------------------------------------------------------------
                     --FUNCIONES AUXILIARES --
--------------------------------------------------------------}
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
valid pf i xs = if (pf == POSTFIX && (i >=0 && (length xs) >= 2 ))
                then True
                else False

swapiaux :: Stack -> Stack
swapiaux (x:y:ys) = (y:x:ys)


cola :: [a] -> [a]
cola [] = []
cola (_:xs) = xs

cabe :: [a] -> a
cabe [] = error "List vacia"
cabe (x:xs) = x

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
                      NGET  -> if(isNat (cabe s))
                               then if((length s)-1 >= comToNat(cabe s))
                                    then True
                                    else False
                               else False

execaux :: Stack -> Stack
exec aux [] = []
execaux (x:xs) = validExec x

validExec :: Command -> Stack
validExec (ES xs) = xs
validExec _ = error "Incompatible"

litS :: Stack -> Bool
litS [] = True
litS (x:xs) = if isNat x
              then True && litS xs
              else False


{-------------------------------------------------------------
                     -- EJEMPLOS --
--------------------------------------------------------------}

seq1 = [I (-1), I 2, ADD, I 3, MUL]

seq2 = [ES [I 2, MUL], EXEC]

prg1 = [ES [I 0, SWAP, SUB], I 7, SWAP, EXEC]

prg2 = [POP]


{-------------------------------------------------------------
                     -- PRUEBAS --
--------------------------------------------------------------}

arith1 = arithOperation (I 1) (I 2) ADD
--Resultado: I 3

arith2 = arithOperation (I 8) (I 0) DIV
--Resultado: *** Exception: Division entre 0.

stack1 = stackOperation [I 1, I 5] SWAP
--Resultado: [I 5, I 1]

stack2 = stackOperation [I 1, I 5] (ES [I 3, ADD, SWAP, I 2])
--Resultado: [ES [I 3, ADD, SWAP, I 2], I 1, I 5]

exec1 = execOperation [ADD] [ES [I 1, ADD], I 2, I 3]
--Resultado: ([I 1, ADD, ADD], [I 2, I 3])

exec2 =  execOperation [MUL] [ES [I 1, ADD], I 2, I 3]
--Resultado: ([I 1, ADD, MUL], [I 2, I 3])

valid1 = validProgram (POSTFIX, 0, [I 1, I 2, ADD]) []
--Resultado: True

valid2 = validProgram (POSTFIX, 2, [ADD]) [I 3, ES[I 1, I 2, SUB]]
--Resultado: False

executeC1 = executeCommands seq1 []
--Resultado: [I 3]

executeC2 = executeCommands seq2 [I 7]
--Resultado: [I 14]

executeP1 = executeProgram (POSTFIX, 0, prg1) []
--Resultado: [I (-7)]

executeP2 = executeProgram (POSTFIX, 1, prg2) [I 4, I 5]
--Resultado: *** Exception: No es un programa válido.
