{-
- Lenguajes de Programación 2019-1
- Recordando Haskell
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Diego Carrillo Verduzco
- Laboratorio: Pablo G. González López
- Emiliano Galeana Araujo 314032324 galeanaara@ciencias.unam.mx
-}

module EjerSem01 where

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
nthElementS n l = case n of
  0 -> headS l
  n -> if n < 0
       then error "Invalid index"
       else case l of
              NilS -> error "Invalid index"
              l -> nthElementS (n-1) (tailS l)

-- | deleteNthElementS. Función que elimina el n-ésimo elemento de la lista.
deleteNthElementS :: Int -> ListS a -> ListS a
deleteNthElementS n l = if n > longS l
                        then NilS
                        else if n < 0
                             then error "Invalid index"
                             else deleteNthElementSAux n l

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
takeS n l = if longS l < n
            then l
            else case n of
                   0 -> NilS
                   n -> case l of
                     NilS -> NilS
                     (Snoc l x) -> addFirstS (headS l) (takeS (n-1) (tailS (Snoc l x)))

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
succN n = case n of
  Zero -> O Zero
  (O x) -> D(succN x)
  (D x) -> (O x)

-- | pred. Función que obtiene el predecesor de un número Nat.
predN :: Nat -> Nat
predN n = case n of
  Zero -> Zero
  O Zero -> Zero
  (O x) -> D x
  (D x) -> O(predN x)

-- | add. Función que obtiene la suma de dos números Nat.
addN :: Nat -> Nat -> Nat
addN n m = case n of
  Zero -> m
  n -> case m of
    Zero -> n
    m -> addN (succN n) (predN m)

-- | prod. Función que obtiene el producto de dos números Nat.
prod :: Nat -> Nat -> Nat
prod Zero _ = Zero
prod _ Zero = Zero
prod n m =
  let
    n1 = mataD n
    m1 = mataD m
  in
    addN m1 (prod (predN n1) m1)

--------------------------------------------------------------------------------
--------                        Funciones auxiliares                    --------
--------------------------------------------------------------------------------

deleteNthElementSAux :: Int -> ListS a -> ListS a
deleteNthElementSAux n l = case n of
  0 -> tailS l
  n -> case l of
    NilS -> NilS
    (Snoc l x) -> addFirstS (headS l) (deleteNthElementSAux (n-1) (tailS (Snoc l x)))

longS :: ListS a -> Int
longS NilS = 0
longS (Snoc l x) = 1 + longS l

mataD :: Nat -> Nat
mataD d = case d of
  Zero -> Zero
  D Zero -> Zero
  O Zero -> O Zero
  (D x) -> D(mataD x)
  (O x) -> O(mataD x)

--------------------------------------------------------------------------------
--------                            Ejemplos                            --------
--------------------------------------------------------------------------------

l = (Snoc (Snoc (Snoc (Snoc (Snoc NilS 1) 2) 3) 4) 5)
u = ( Snoc ( Snoc ( Snoc NilS 1 ) 2 )3 )
d = ( Snoc ( Snoc ( Snoc NilS 6 ) 7 ) 8 )


--------------------------------------------------------------------------------
--------                             Pruebas                            --------
--------------------------------------------------------------------------------

headS1 = headS NilS
--Resultado: ***Exception: Empty list

headS2 = headS l
--Resultado: 1

tailS1 = tailS NilS
--Resultado: ***Exception: Empty list

tailS2 = tailS (Snoc NilS 1)
--Resultado: NilS

tailS3 = tailS l
--Resultado: Snoc(Snoc(Snoc(Snoc NilS 2)3)4)5

initS1 = initS NilS
--Resultado: ***Exception: Empty list

initS2 = initS (Snoc NilS 1)
--Resultado: NilS

initS3 = initS l
--Resultado: Snoc(Snoc(Snoc(Snoc NilS 1)2)3)4

lastS1 = lastS NilS
--Resultado: ***Exception Empty list

lastS2 = lastS l
--Resultado: 5

nthElementS1 = nthElementS 5 NilS
--Resultado: ***Exception Invalid index

nthElementS2 = nthElementS 10 l
--Resultado: ***Exception Invalid index

nthElementS3 = nthElementS (-1) l
--Resultado: ***Exception Invalid index

nthElementS4 = nthElementS 0 l
--Resultado: 1

nthElementS5 = nthElementS 2 l
--Resultado: 3

deleteNthElementS1 = deleteNthElementS 5 NilS
--Resultado: NilS

deleteNthElementS2 = deleteNthElementS 10 l
--Resultado: NilS

deleteNthElementS3 = deleteNthElementS (-1) l
--Resultado: ***Exception: Invalid index

deleteNthElementS4 = deleteNthElementS 2 l
--Resultado: Snoc(Snoc(Snoc(Snoc NilS 1)2)4)5

addFirstS1 = addFirstS 0 l
--Resultado: Snoc(Snoc(Snoc(Snoc(Snoc(Snoc NilS 0)1)2)3)4)5

addLastS1 = addLastS 6 l
--Resultado: Snoc(Snoc(Snoc(Snoc(Snoc(Snoc NilS 1)2)3)4)5)6

reverseS1 = reverseS l
--Resultado: Snoc(Snoc(Snoc(Snoc(Snoc NilS 5)4)3)2)1

appendS1 = appendS u d
--Resultado: Snoc(Snoc(Snoc(Snoc(Snoc(Snoc NilS 1)2)3)6)7)8

takeS1 = takeS 0 l
--Resultado: NilS

takeS2 = takeS 10 l
--Resultado: Snoc(Snoc(Snoc(Snoc(Snoc NilS 1)2)3)4)5

takeS3 = takeS 2 l
--Resultado: Snoc(Snoc NilS 1)2

toNat1 = toNat 616
--Resultado: D (D (D (O (D (O (O (D (D (O Zero)))))))))

succN1 = succN (D(O(D Zero)))
--Resultado: O (O Zero)

predN1 = predN (D(O Zero))
--Resultado: O Zero

addN1 = addN (D(O(D Zero))) (D(O Zero))
--Resultado: D (D (O Zero))

prod1 = prod (D(O(D Zero))) (D(O Zero))
--Resultado: D (D (O Zero))
