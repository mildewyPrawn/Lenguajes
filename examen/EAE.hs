module EAE where

data E = N Int | Sum E E | Res E E | Mul E E | Div E E | Tup E E
       | NegNExec E | MaxNExec E | DivZExec E E | RemExec E E deriving (Show, Eq)

--evs :: EAE -> Int
evs (N n) = (N n)
evs (Sum (N n) (N m)) = N (n + m)
evs (Sum (N n) e1) = (Sum (N n) (evs e1))
evs (Sum e1 e2) = Sum (evs e1) e2
evs (Res (N n) (N m)) = N (n - m)
evs (Res (N n) e1) = Res (N n) (evs e1)
evs (Res e1 e2) = Res (evs e1) e2
evs (Mul (N n) (N m)) = N (n * m)
evs (Mul (N n) e1) = Mul (N n) (evs e1)
evs (Mul e1 e2) = Mul (evs e1) e2
evs (Div (N n) (N m)) = N (n `div` m)
evs (Div (N n) (N m)) = evs (DivZExec (N n) (N m))
evs (Div (N n) e1) = Div (N n) (evs e1)
evs (Div e1 e2) = Div (evs e1) e2

evs (NegNExec (N n)) = if n < 0
                       then (N n)
                       else (N n)
evs (NegNExec e1) = NegNExec (evs e1)

evs (MaxNExec (N n)) = if n > 1000
                       then error "Maximo exedido"
                       else (N n)
evs (MaxNExec e1) = MaxNExec (evs e1)

evs (DivZExec (N _) (N 0)) = error "Division entre 0"
evs (DivZExec (N n) (N m)) = N (n `div` m)

evs (RemExec (N n) (N m)) = if m `mod` n == 0
                            then N (n `div` m)
                            else Tup (N n) (N m)

evs (Tup (N n) (N m)) = Tup (N n) (N m)


--eve :: EAE -> Int

eve n = evs (NegNExec(MaxNExec (evs n)))
