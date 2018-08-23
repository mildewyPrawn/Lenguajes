{-
- Lenguajes de Programación 2019-1
- Implementación de Postfix
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
-}

module Practica1 where

data Command = I Int | ADD | DIV | Eq | EXEC | Gt | Lt | MUL | NGET | POP | REM |
               SEL | SUB | SWAP | ES [Command] deriving Show

data PF = POSTFIX deriving (Show, Eq)

type Program = (PF, Int, [Command])

type Stack = [Command]

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

stackOperation :: Stack -> Command -> Stack
--stackOperation = error " D: "

--literal E, nget, pop, sel, swap, secE
-- xs      , n   ,  1 ,  3 ,   2 ,   xs
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
                         NGET  -> if (validaStack s com)
                                  then ngetaux s (comToNat (cab s))  -- esto no sé que tan bien esté
                                  else error "Error en Stack, faltan elementos"

ngetaux :: Stack -> Int -> Stack
ngetaux (x:xs) = error "Tenemos que hacer que cuente los n elementos, no entiendo nget "

selaux :: Stack -> Stack
selaux (x:y:z:zs) = if (comToNat z == 0)
                    then (x:zs)
                    else (y:zs)

comToNat :: Command -> Int
comToNat (I n) = n
comToNat _ = error "Tipo de dato no aceptado."

swapiaux :: Stack -> Stack
swapiaux (x:y:ys) = (y:x:ys)

cola :: [a] -> [a]
cola [] = []
cola (_:xs) = xs

cab :: [a] -> a
cab (x:xs) = x

validaStack :: Stack -> Command -> Bool
validaStack s com = case com of
                      POP   -> if (long s) > 0
                               then True
                               else False
                      SWAP  -> if (long s) > 1
                               then True
                               else False
                      SEL   -> if (long s) > 2
                               then True
                               else False
                      NGET  -> if (long (cola s) > comToNat (cab s))
                               then True
                               else False

long :: Stack -> Int
long [] = 0
long (x:xs) = 1 + long xs

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


