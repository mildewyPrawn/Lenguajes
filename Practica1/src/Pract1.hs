{-
- Lenguajes de Programación 2019-1
- Recordando Haskell
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
-}

module Pract1 where

data ListS a = NilS | Snoc (ListS a) a deriving Show

-- | NilS representa la lista vacía.
-- | Snoc representa el operador agrega al final de la lista un elemento.

-- La lista Snoc (Snoc (Snoc (Snoc (Snoc NilS 1) 2) 3) 4) 5 se ve
-- de este modo:
-- ((((<> :: 1) :: 2) :: 3) :: 4) :: 5
-- El elemento 1 es el primer elemento y el elemento 5 el último.

-- | headS. Función que obtiene el primer elemento de la lista.
headS :: ListS a -> a
headS l = case l of
  NilS -> error "Empty list"
  (Snoc l' x) -> case l' of
    NilS -> x
    (Snoc l'' y) -> headS l'

-- | tailS. Función que obtiene la lista sin el primer elemento.
tailS :: ListS a -> ListS a
tailS l = case l of
  NilS -> error "Empty list"
  (Snoc l' x) -> case l' of
    NilS -> NilS
    (Snoc l'' y) -> Snoc (tailS l') x

-- | initS. Función que obtiene la lista sin el primer elemento.
initS :: ListS a -> ListS a
initS l = case l of
  NilS -> error "Empty list"
  (Snoc l' _) -> l'

-- | lastS. Función que obtiene el último elemento de la lista.
lastS :: ListS a -> a
lastS l = case l of
  NilS -> error "Empty list"
  (Snoc _ x) -> x

-- | nthElementS. Función que regresa el n-ésimo elemento de la lista.
nthElementS :: Int -> ListS a -> a
nthElementS n l = case l of
  NilS -> error "Invalid index"
  (Snoc l x) -> case n of
    0 -> headS(Snoc l x)
    n -> nthElementS (n-1) (tailS (Snoc l x))

-- | deleteNthElementS. Función que elimina el n-ésimo elemento de la lista.
deleteNthElementS :: Int -> ListS a -> ListS a
deleteNthElementS = error "Implementar"
{-
deleteNthElementS 0 (Snoc xs a) = tailS (Snoc xs a)
deleteNthElementS n NilS = NilS
--deleteNthElementS n (Snoc xs a) = deleteNthElementS (n-1) (tailS (Snoc xs a))
--deleteNthElementS n (Snoc xs a) = Snoc (deleteNthElementS (n-1) (tailS (Snoc xs a))) (headS (Snoc xs a))
deleteNthElementS n (Snoc l x) = 
-}
-- | addFirstS. Función que obtiene la lista donde el primer elemento es el
-- | elemento dado.
addFirstS :: a -> ListS a -> ListS a
addFirstS x l = case l of
  NilS -> Snoc NilS x
  (Snoc l' y) -> Snoc (addFirstS x l') y

-- | addLastS. Función que obtiene la lista donde el último elemento es el
-- | elemento dado.
addLastS :: a -> ListS a -> ListS a
addLastS x l = case l of
  NilS -> Snoc NilS x
  (Snoc l' y) -> Snoc (Snoc l' y) x

-- | reverseS. Función que obtiene la reversa de la lista.
reverseS :: ListS a -> ListS a
reverseS l = case l of
  NilS -> NilS
  (Snoc l x) -> Snoc (reverseS (tailS (Snoc l x))) (headS  (Snoc l x))

-- | appendS. Función que obtiene la concatenación de dos listas.
appendS :: ListS a -> ListS a -> ListS a
appendS l l2 = case l of
  NilS -> l2
  (Snoc l' x) -> case l2 of
    NilS -> l
    (Snoc l'' y) -> appendS(addLastS(headS(Snoc l'' y)) (Snoc l' x)) (tailS(Snoc l'' y))

-- | takeS. Función que obtiene la lista con los primeros n elementos.
takeS :: Int -> ListS a -> ListS a
takeS = error "Implemetar"
{-
takeS n l = case n of
  0 -> NilS
  n -> case l of
-}
--takeS 0 (Snoc xs x) = NilS
--takes n (Snoc xs x) =
--takeS n (Snoc xs x) = Snoc (takeS (n-1) (tailS (Snoc xs x))) (headS (Snoc xs x))


l = (Snoc (Snoc (Snoc (Snoc (Snoc NilS 1) 2) 3) 4) 5)
u = ( Snoc ( Snoc ( Snoc NilS 1 ) 2 )3 )
d = ( Snoc ( Snoc ( Snoc NilS 6 ) 7 ) 8 )


data Nat = Zero | D (Nat) | O (Nat) deriving Show

-- | Zero representa en número cero (0).
-- | Dx representa al doble de x, con x un número natural (2x).
-- | Ox representa al sucesor del doble de x, con x un número natural (2x+1).

-- | toNat. Función que obtiene la representación en números Nata de un número
-- | entero.
toNat :: Int -> Nat
toNat 0 = Zero
toNat n = if n `mod` 2 == 0
          then D(toNat (div n 2))
          else O(toNat (div (n-1) 2))

-- | succ. Función que obtiene el sucesor de un número Nat.
succN :: Nat -> Nat
succN nat = case nat of
  Zero -> O Zero
  (O x) -> mataD(D (succN x))
  (D x) -> mataD(O x)

mataD :: Nat -> Nat
mataD nat = case nat of
  (D Zero) -> Zero
  (O Zero) -> O Zero
  (D x) -> D(mataD x)
  (O x) -> O(mataD x)

-- | pred. Función que obtiene el predecesor de un número Nat.
predN :: Nat -> Nat
predN nat = case nat of
  Zero -> error "Invalid index"
  (O x) -> mataD(D x)
  (D x) -> mataD(O (predN x))

-- | add. Función que obtiene la suma de dos números Nat.
add :: Nat -> Nat -> Nat
add n m = case n of
  Zero -> case m of
    Zero -> Zero
    m -> m
  n -> case m of
    Zero -> n
    m -> add (succN n) (predN m)

{-
add Zero Zero = Zero
add n Zero = n
add Zero m = m
add n m = add (succN n) (predN m)
-}

-- | prod. Función que obtiene el producto de dos números Nat.
prod :: Nat -> Nat -> Nat
prod 
--prod = error "Implementar"

